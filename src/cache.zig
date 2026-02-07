//! Cache manager: stores and retrieves compiled grammar .so files.
//!
//! Cache location: ~/.cache/zgram/<hash>.so
//! Hash key: SHA256(grammar_text)

const std = @import("std");
const Sha256 = std.crypto.hash.sha2.Sha256;

/// Compute SHA256 hex digest of grammar text.
pub fn grammarHash(text: []const u8, out: *[64]u8) void {
    var digest: [32]u8 = undefined;
    Sha256.hash(text, &digest, .{});
    const hex_chars = "0123456789abcdef";
    for (digest, 0..) |byte, i| {
        out[i * 2] = hex_chars[byte >> 4];
        out[i * 2 + 1] = hex_chars[byte & 0x0f];
    }
}

/// Build the full cache path for a given hash into the provided buffer.
/// Returns the valid slice, or error if HOME is not set or path too long.
fn buildCachePath(hash: []const u8, buf: *[512]u8) ![]const u8 {
    const home = std.posix.getenv("HOME") orelse return error.HomeNotSet;
    var pos: usize = 0;

    if (pos + home.len > buf.len) return error.PathTooLong;
    @memcpy(buf[pos..][0..home.len], home);
    pos += home.len;

    const suffix = "/.cache/zgram/";
    if (pos + suffix.len > buf.len) return error.PathTooLong;
    @memcpy(buf[pos..][0..suffix.len], suffix);
    pos += suffix.len;

    if (pos + hash.len > buf.len) return error.PathTooLong;
    @memcpy(buf[pos..][0..hash.len], hash);
    pos += hash.len;

    const ext = ".so";
    if (pos + ext.len > buf.len) return error.PathTooLong;
    @memcpy(buf[pos..][0..ext.len], ext);
    pos += ext.len;

    return buf[0..pos];
}

/// Build just the cache directory path.
fn buildCacheDir(buf: *[512]u8) ![]const u8 {
    const home = std.posix.getenv("HOME") orelse return error.HomeNotSet;
    var pos: usize = 0;

    if (pos + home.len > buf.len) return error.PathTooLong;
    @memcpy(buf[pos..][0..home.len], home);
    pos += home.len;

    const suffix = "/.cache/zgram";
    if (pos + suffix.len > buf.len) return error.PathTooLong;
    @memcpy(buf[pos..][0..suffix.len], suffix);
    pos += suffix.len;

    return buf[0..pos];
}

/// Ensure the cache directory exists.
fn ensureCacheDir() !void {
    var buf: [512]u8 = undefined;
    const dir_path = try buildCacheDir(&buf);
    // makePath expects a null-terminated or slice path
    std.fs.cwd().makePath(dir_path) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };
}

/// Check if a cached .so exists for the given hash.
/// Returns the path (in caller-provided buffer) if found, null otherwise.
pub fn getCached(hash: []const u8, path_buf: *[512]u8) ?[]const u8 {
    const path = buildCachePath(hash, path_buf) catch return null;
    std.fs.cwd().access(path, .{}) catch return null;
    return path;
}

/// Copy a compiled .so into the cache. Returns the cached path.
pub fn storeCached(hash: []const u8, so_path: []const u8, path_buf: *[512]u8) ![]const u8 {
    try ensureCacheDir();
    const dest_path = try buildCachePath(hash, path_buf);

    // Write to a temp file first, then atomic rename to prevent partial reads
    var tmp_buf: [512]u8 = undefined;
    const tmp_path = std.fmt.bufPrint(&tmp_buf, "{s}.tmp", .{dest_path}) catch return error.PathTooLong;

    // Copy file using std.fs
    const src_file = try std.fs.cwd().openFile(so_path, .{});
    defer src_file.close();

    // Create temp file with explicit permissions (owner rwx, group/other rx)
    const tmp_file = try std.fs.cwd().createFile(tmp_path, .{ .mode = 0o755 });
    errdefer {
        tmp_file.close();
        std.fs.cwd().deleteFile(tmp_path) catch {};
    }

    // Copy contents
    var buf: [8192]u8 = undefined;
    while (true) {
        const n = try src_file.read(&buf);
        if (n == 0) break;
        try tmp_file.writeAll(buf[0..n]);
    }
    tmp_file.close();

    // Atomic rename to final path
    std.fs.cwd().rename(tmp_path, dest_path) catch |err| {
        std.fs.cwd().deleteFile(tmp_path) catch {};
        return err;
    };

    return dest_path;
}

/// Remove all cached .so files. Returns count of files actually removed.
pub fn clearCache() !i64 {
    var dir_buf: [512]u8 = undefined;
    const dir_path = buildCacheDir(&dir_buf) catch return 0;

    var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch return 0;
    defer dir.close();

    var total_count: i64 = 0;

    // Loop in batches to handle caches with >1024 entries
    while (true) {
        var names: [1024][256]u8 = undefined;
        var name_lens: [1024]usize = undefined;
        var name_count: usize = 0;

        var iter = dir.iterate();
        while (iter.next() catch null) |entry| {
            if (entry.kind != .file) continue;
            if (!std.mem.endsWith(u8, entry.name, ".so") and
                !std.mem.endsWith(u8, entry.name, ".so.tmp")) continue;
            if (name_count >= 1024) break;
            const len = @min(entry.name.len, 256);
            @memcpy(names[name_count][0..len], entry.name[0..len]);
            name_lens[name_count] = len;
            name_count += 1;
        }

        if (name_count == 0) break;

        for (0..name_count) |i| {
            dir.deleteFile(names[i][0..name_lens[i]]) catch continue;
            total_count += 1;
        }

        // If we collected fewer than 1024, we've seen all files
        if (name_count < 1024) break;
    }

    return total_count;
}
