//! Compiler orchestrator: scaffolds a temp Zig project and builds a grammar .so.
//!
//! Build directory layout:
//!     <tmpdir>/zgram_build_<hash16>/
//!         src/
//!             grammar_data.zig   (codegen output)
//!             grammar_lib.zig    (template)
//!             grammar_types.zig  (template)
//!             parse_abi.zig      (template)
//!             parser_engine.zig  (template)
//!         build.zig
//!         build.zig.zon

const std = @import("std");
const Allocator = std.mem.Allocator;
const templates = @import("templates.zig");
const builtin = @import("builtin");

const LIB_NAME_PLACEHOLDER = "LIB_NAME_PLACEHOLDER";

/// Get the system temporary directory path, cross-platform.
fn getTmpDir() []const u8 {
    if (builtin.os.tag == .windows) {
        return std.posix.getenv("TEMP") orelse std.posix.getenv("TMP") orelse "C:\\Temp";
    }
    return std.posix.getenv("TMPDIR") orelse "/tmp";
}

/// Compile a grammar_data.zig source into a .so file.
/// Returns the path to the compiled .so (caller must free).
pub fn compileGrammar(allocator: Allocator, grammar_data_zig: []const u8, hash: []const u8) ![]const u8 {
    const hash16 = hash[0..@min(hash.len, 16)];
    const tmp_dir = getTmpDir();

    // Build the temp directory path
    var build_dir_buf: [512]u8 = undefined;
    const build_dir_path = try std.fmt.bufPrint(&build_dir_buf, "{s}/zgram_build_{s}", .{ tmp_dir, hash16 });

    // Build the lib name: zgram_<hash16>
    var lib_name_buf: [128]u8 = undefined;
    const lib_name = try std.fmt.bufPrint(&lib_name_buf, "zgram_{s}", .{hash16});

    // Create build directory and src subdirectory
    try std.fs.cwd().makePath(build_dir_path);

    var src_dir_buf: [256]u8 = undefined;
    const src_dir_path = try std.fmt.bufPrint(&src_dir_buf, "{s}/src", .{build_dir_path});
    try std.fs.cwd().makePath(src_dir_path);

    // Write grammar_data.zig
    try writeFile(build_dir_path, "src/grammar_data.zig", grammar_data_zig);

    // Write template files
    try writeFile(build_dir_path, "src/grammar_lib.zig", templates.grammar_lib_zig);
    try writeFile(build_dir_path, "src/grammar_types.zig", templates.grammar_types_zig);
    try writeFile(build_dir_path, "src/parser_engine.zig", templates.parser_engine_zig);
    try writeFile(build_dir_path, "src/parse_abi.zig", templates.parse_abi_zig);

    // Write build.zig with lib name substituted
    const build_zig = try replaceAll(allocator, templates.build_zig_template, LIB_NAME_PLACEHOLDER, lib_name);
    defer allocator.free(build_zig);
    try writeFile(build_dir_path, "build.zig", build_zig);

    // Write build.zig.zon
    const build_zig_zon = try replaceAll(allocator, templates.build_zig_zon_template, LIB_NAME_PLACEHOLDER, lib_name);
    defer allocator.free(build_zig_zon);
    try writeFile(build_dir_path, "build.zig.zon", build_zig_zon);

    // First build attempt — may fail requesting a fingerprint
    var stderr_buf: [8192]u8 = undefined;
    var result = try runZigBuild(build_dir_path, &stderr_buf);

    if (result.exit_code != 0) {
        // Check for fingerprint error and patch
        const stderr_output = stderr_buf[0..result.stderr_len];
        if (findFingerprint(stderr_output)) |fingerprint| {
            // Patch build.zig.zon with fingerprint
            try patchFingerprint(allocator, build_dir_path, fingerprint);

            // Retry build
            result = try runZigBuild(build_dir_path, &stderr_buf);
        }
    }

    if (result.exit_code != 0) {
        // Log stderr so the caller can see what went wrong
        const stderr_output = stderr_buf[0..result.stderr_len];
        if (stderr_output.len > 0) {
            std.log.err("zig build failed:\n{s}", .{stderr_output});
        }
        return error.CompilationFailed;
    }

    // Build the output .so path
    const so_path = try std.fmt.allocPrint(allocator, "{s}/zig-out/lib/{s}.so", .{ build_dir_path, lib_name });

    // Verify the file exists
    std.fs.cwd().access(so_path, .{}) catch {
        allocator.free(so_path);
        return error.OutputNotFound;
    };

    return so_path;
}

/// Clean up a temp build directory. Call after the .so has been cached.
pub fn cleanupBuildDir(hash: []const u8) void {
    const hash16 = hash[0..@min(hash.len, 16)];
    const tmp_dir = getTmpDir();
    var buf: [512]u8 = undefined;
    const path = std.fmt.bufPrint(&buf, "{s}/zgram_build_{s}", .{ tmp_dir, hash16 }) catch return;
    std.fs.cwd().deleteTree(path) catch {};
}

fn writeFile(dir_path: []const u8, name: []const u8, content: []const u8) !void {
    var path_buf: [512]u8 = undefined;
    const full_path = try std.fmt.bufPrint(&path_buf, "{s}/{s}", .{ dir_path, name });

    const file = try std.fs.cwd().createFile(full_path, .{});
    defer file.close();
    try file.writeAll(content);
}

const BuildResult = struct {
    exit_code: u8,
    stderr_len: usize,
};

fn runZigBuild(build_dir_path: []const u8, stderr_buf: *[8192]u8) !BuildResult {
    var child = std.process.Child.init(
        &.{ "zig", "build", "-Doptimize=ReleaseFast" },
        std.heap.page_allocator,
    );
    child.cwd = build_dir_path;

    child.stderr_behavior = .Pipe;
    child.stdout_behavior = .Pipe;

    try child.spawn();

    // Read stderr
    var stderr_len: usize = 0;
    if (child.stderr) |stderr_pipe| {
        stderr_len = stderr_pipe.readAll(stderr_buf) catch 0;
    }

    // Drain stdout
    if (child.stdout) |stdout_pipe| {
        var discard: [4096]u8 = undefined;
        while (true) {
            const n = stdout_pipe.read(&discard) catch break;
            if (n == 0) break;
        }
    }

    const term = try child.wait();
    const exit_code: u8 = switch (term) {
        .Exited => |code| code,
        else => 1,
    };

    return .{ .exit_code = exit_code, .stderr_len = stderr_len };
}

/// Search for a fingerprint value like "0x1234abcd" in zig build stderr.
fn findFingerprint(stderr: []const u8) ?[]const u8 {
    // Look for "suggested value: 0x..."
    const needle = "suggested value: ";
    var pos: usize = 0;
    while (pos + needle.len < stderr.len) {
        if (std.mem.eql(u8, stderr[pos..][0..needle.len], needle)) {
            const start = pos + needle.len;
            var end = start;
            // Read the hex value (0x followed by hex digits)
            if (end < stderr.len and stderr[end] == '0') {
                end += 1;
                if (end < stderr.len and stderr[end] == 'x') {
                    end += 1;
                    while (end < stderr.len and isHexDigit(stderr[end])) {
                        end += 1;
                    }
                    if (end > start + 2) {
                        return stderr[start..end];
                    }
                }
            }
        }
        pos += 1;
    }
    return null;
}

fn isHexDigit(c: u8) bool {
    return (c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F');
}

/// Patch build.zig.zon to insert the fingerprint after .version line.
fn patchFingerprint(allocator: Allocator, build_dir_path: []const u8, fingerprint: []const u8) !void {
    var path_buf: [512]u8 = undefined;
    const zon_path = try std.fmt.bufPrint(&path_buf, "{s}/build.zig.zon", .{build_dir_path});

    const file = try std.fs.cwd().openFile(zon_path, .{ .mode = .read_only });
    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    file.close();
    defer allocator.free(content);

    // Find '.version = "0.0.1",' and insert fingerprint after it
    const version_needle = ".version = \"0.0.1\",";
    const insert_pos = std.mem.indexOf(u8, content, version_needle) orelse return error.VersionNotFound;
    const after_version = insert_pos + version_needle.len;

    // Build patched content
    var patched: std.ArrayList(u8) = .empty;
    defer patched.deinit(allocator);

    try patched.appendSlice(allocator, content[0..after_version]);
    try patched.appendSlice(allocator, "\n    .fingerprint = ");
    try patched.appendSlice(allocator, fingerprint);
    try patched.appendSlice(allocator, ",");
    try patched.appendSlice(allocator, content[after_version..]);

    // Write back
    const out_file = try std.fs.cwd().createFile(zon_path, .{});
    defer out_file.close();
    try out_file.writeAll(patched.items);
}

/// Replace all occurrences of `needle` with `replacement` in `input`.
fn replaceAll(allocator: Allocator, input: []const u8, needle: []const u8, replacement: []const u8) ![]const u8 {
    var result: std.ArrayList(u8) = .empty;
    defer result.deinit(allocator);

    var pos: usize = 0;
    while (pos < input.len) {
        if (pos + needle.len <= input.len and std.mem.eql(u8, input[pos..][0..needle.len], needle)) {
            try result.appendSlice(allocator, replacement);
            pos += needle.len;
        } else {
            try result.append(allocator, input[pos]);
            pos += 1;
        }
    }

    return try allocator.dupe(u8, result.items);
}
