//! Compiled grammars kept between processes: each parser module's object
//! file (LLVM's optimizing and code generation, nearly all of compiling a
//! grammar), on disk by what it compiles.
//!
//! The key is the module's bitcode before it is optimized (what zgram's
//! code generator made of the grammar, for the parser kind), salted with
//! what the object depends on besides: zgram's version and cache format,
//! LLVM's version, the host's triple, CPU and features. The module calls
//! zgram's helpers by name, linked when the object is loaded: an object
//! made in another process for the same bitcode is the code compiling this
//! one would make.
//!
//! A file is the object's SHA-256, then the object: checked when read (a
//! file damaged or cut short is compiled again, and replaced).
//!
//! Where: the platform's place for caches (Windows: %LOCALAPPDATA%\zgram\Cache;
//! macOS: ~/Library/Caches/zgram; else $XDG_CACHE_HOME/zgram or
//! ~/.cache/zgram), or where zgram.configure(cache=path) says (cache=False:
//! none). It takes at most `limit` bytes: past it, the objects used least
//! recently go.

const std = @import("std");
const builtin = @import("builtin");

/// Bump when what the objects mean changes without the bitcode showing it
pub const format = "zgram-cache-1";

pub const Key = [32]u8;

/// Where the cache is (zgram.configure(cache=...)): the platform's usual
/// place, none, or a directory given
pub const Setting = union(enum) { default, off, dir: []const u8 };

var setting: Setting = .default;
var resolved = false;
var dir_path: ?[]const u8 = null;
/// Settings and the directory's resolution, from any thread
var lock: std.atomic.Mutex = .unlocked;

fn acquire() void {
    while (!lock.tryLock()) std.atomic.spinLoopHint();
}

/// The platform's place for caches, zgram's directory in it (owned by the
/// caller), or null if there's no home to put it in.
fn defaultDir(a: std.mem.Allocator) ?[]u8 {
    switch (builtin.os.tag) {
        .windows => {
            if (env(a, "LOCALAPPDATA")) |base| {
                defer a.free(base);
                return std.fmt.allocPrint(a, "{s}\\zgram\\Cache", .{base}) catch null;
            }
            const home = env(a, "USERPROFILE") orelse return null;
            defer a.free(home);
            return std.fmt.allocPrint(a, "{s}\\AppData\\Local\\zgram\\Cache", .{home}) catch null;
        },
        .macos => {
            const home = env(a, "HOME") orelse return null;
            defer a.free(home);
            return std.fmt.allocPrint(a, "{s}/Library/Caches/zgram", .{home}) catch null;
        },
        else => {
            // (XDG's must be absolute: a relative one is ignored)
            if (env(a, "XDG_CACHE_HOME")) |base| {
                defer a.free(base);
                if (std.fs.path.isAbsolute(base)) return std.fmt.allocPrint(a, "{s}/zgram", .{base}) catch null;
            }
            const home = env(a, "HOME") orelse return null;
            defer a.free(home);
            return std.fmt.allocPrint(a, "{s}/.cache/zgram", .{home}) catch null;
        },
    }
}

/// An environment variable of the process, not empty (owned by the
/// caller), or null.
fn env(a: std.mem.Allocator, key: []const u8) ?[]u8 {
    // (Windows: the process's block, read each time; elsewhere libc's)
    const environ: std.process.Environ = if (builtin.os.tag == .windows)
        .{ .block = .global }
    else
        .{ .block = .{ .slice = @ptrCast(std.mem.span(std.c.environ)) } };
    const v = environ.getAlloc(a, key) catch return null;
    if (v.len == 0) {
        a.free(v);
        return null;
    }
    return v;
}

/// Blocking file calls: the platform's
var threaded: std.Io.Threaded = .init_single_threaded;

fn io() std.Io {
    return threaded.io();
}

/// The cache from now on (the directory given: the caller's, copied).
pub fn set(s: Setting) !void {
    const a = std.heap.c_allocator;
    const copy: Setting = switch (s) {
        .dir => |d| .{ .dir = try a.dupe(u8, d) },
        else => s,
    };
    acquire();
    defer lock.unlock();
    if (setting == .dir) a.free(setting.dir);
    setting = copy;
    resolved = false;
    if (dir_path) |p| a.free(p);
    dir_path = null;
}

/// The cache's directory (made if needed), or null: no cache. (Kept until
/// the setting changes: zgram.configure() isn't called while grammars
/// compile on other threads.)
pub fn dir() ?[]const u8 {
    acquire();
    defer lock.unlock();
    if (resolved) return dir_path;
    resolved = true;
    const a = std.heap.c_allocator;
    const path = switch (setting) {
        .off => return null,
        .dir => |d| a.dupe(u8, d) catch return null,
        .default => defaultDir(a) orelse return null,
    };
    std.Io.Dir.cwd().createDirPath(io(), path) catch {
        a.free(path);
        return null;
    };
    dir_path = path;
    return path;
}

/// Where the object of `key` is kept (`buf`'s), or null: no cache.
fn pathFor(buf: []u8, key: Key) ?[]const u8 {
    const d = dir() orelse return null;
    return std.fmt.bufPrint(buf, "{s}" ++ std.fs.path.sep_str ++ "{s}.o", .{ d, std.fmt.bytesToHex(key, .lower) }) catch null;
}

/// The object kept for `key` (owned by the caller), or null; one found is
/// used now (its time made now: the least recently used go first).
pub fn read(a: std.mem.Allocator, key: Key) ?[]u8 {
    var buf: [4096]u8 = undefined;
    const path = pathFor(&buf, key) orelse return null;
    const bytes = std.Io.Dir.cwd().readFileAlloc(io(), path, a, .unlimited) catch return null;
    if (bytes.len <= 32 or !std.mem.eql(u8, bytes[0..32], &digest(bytes[32..]))) {
        a.free(bytes);
        return null;
    }
    // (through the file: Zig 0.16's Dir.setTimestamps isn't there on Windows)
    if (std.Io.Dir.cwd().openFile(io(), path, .{ .mode = .read_write })) |f| {
        defer f.close(io());
        f.setTimestampsNow(io()) catch {};
    } else |_| {}
    std.mem.copyForwards(u8, bytes[0 .. bytes.len - 32], bytes[32..]);
    return a.realloc(bytes, bytes.len - 32) catch bytes[0 .. bytes.len - 32];
}

/// Keep `object` for `key` (whole or not at all). Failing is fine:
/// compiled again next time.
pub fn write(key: Key, object: []const u8) void {
    var buf: [4096]u8 = undefined;
    const path = pathFor(&buf, key) orelse return;
    const a = std.heap.c_allocator;
    const bytes = a.alloc(u8, 32 + object.len) catch return;
    defer a.free(bytes);
    bytes[0..32].* = digest(object);
    @memcpy(bytes[32..], object);
    writeWhole(path, bytes) catch return;
    wrote(bytes.len);
}

fn digest(bytes: []const u8) [32]u8 {
    var out: [32]u8 = undefined;
    std.crypto.hash.sha2.Sha256.hash(bytes, &out, .{});
    return out;
}

/// Write a file whole or not at all: written aside (a name of this
/// thread's), then renamed over `path` (replacing it, on every platform).
fn writeWhole(path: []const u8, bytes: []const u8) !void {
    var buf: [4096]u8 = undefined;
    const pid: u64 = if (builtin.os.tag == .windows) std.os.windows.GetCurrentProcessId() else @intCast(std.c.getpid());
    const tmp = try std.fmt.bufPrint(&buf, "{s}.{d}.{d}.tmp", .{ path, pid, std.Thread.getCurrentId() });
    const here = std.Io.Dir.cwd();
    try here.writeFile(io(), .{ .sub_path = tmp, .data = bytes });
    here.rename(tmp, here, path, io()) catch |e| {
        here.deleteFile(io(), tmp) catch {};
        return e;
    };
}

// ----------------------------------------------------------------------
// The cache's size
// ----------------------------------------------------------------------

/// The most the cache's objects take, in bytes (zgram.configure(cache_size=)):
/// past it the least recently used go, down to 80% of it. 0: no limit.
pub var limit: u64 = 256 << 20;

/// Bytes written since the cache was last looked over (by this process)
var written: u64 = 0;
var looked_over = false;
var size_lock: std.atomic.Mutex = .unlocked;

/// After an object is written: the cache looked over the first time this
/// process writes one, and each time it has written a tenth of the limit
/// since (other processes write too: the first look sees theirs).
fn wrote(n: usize) void {
    if (limit == 0) return;
    while (!size_lock.tryLock()) std.atomic.spinLoopHint();
    defer size_lock.unlock();
    written += n;
    if (looked_over and written < limit / 10) return;
    looked_over = true;
    written = 0;
    trim(limit);
}

const Entry = struct { name: []u8, size: u64, time: i96 };

/// The cache down to 80% of `max` bytes if it's over `max`, the least
/// recently used objects deleted first; files a write left behind (a
/// process stopped while writing) deleted when they're an hour old. What
/// can't be done is left (another process deleting the same, a file open
/// on Windows).
pub fn trim(max: u64) void {
    const d = dir() orelse return;
    const a = std.heap.c_allocator;
    var folder = std.Io.Dir.cwd().openDir(io(), d, .{ .iterate = true }) catch return;
    defer folder.close(io());
    var entries: std.ArrayListUnmanaged(Entry) = .empty;
    defer {
        for (entries.items) |e| a.free(e.name);
        entries.deinit(a);
    }
    const now = std.Io.Timestamp.now(io(), .real).nanoseconds;
    const hour: i96 = 3600 * std.time.ns_per_s;
    var total: u64 = 0;
    var it = folder.iterate();
    while (it.next(io()) catch return) |e| {
        if (e.kind != .file) continue;
        const tmp = std.mem.endsWith(u8, e.name, ".tmp");
        if (!tmp and !std.mem.endsWith(u8, e.name, ".o")) continue;
        const st = folder.statFile(io(), e.name, .{}) catch continue;
        if (tmp) {
            if (now - st.mtime.nanoseconds > hour) folder.deleteFile(io(), e.name) catch {};
            continue;
        }
        total += st.size;
        const name = a.dupe(u8, e.name) catch return;
        entries.append(a, .{ .name = name, .size = st.size, .time = st.mtime.nanoseconds }) catch {
            a.free(name);
            return;
        };
    }
    if (total <= max) return;
    std.mem.sort(Entry, entries.items, {}, struct {
        fn older(_: void, x: Entry, y: Entry) bool {
            return x.time < y.time;
        }
    }.older);
    const goal = max / 10 * 8;
    for (entries.items) |e| {
        if (total <= goal) break;
        folder.deleteFile(io(), e.name) catch continue;
        total -= e.size;
    }
}
