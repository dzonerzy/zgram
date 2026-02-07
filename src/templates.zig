//! Embedded Zig template strings for grammar .so builds.
//!
//! These are the source files that get written into temp build directories
//! when compiling a grammar into a standalone .so.

/// grammar_types.zig — comptime grammar data structures
pub const grammar_types_zig = @embedFile("zig_templates/grammar_types.zig");

/// parser_engine.zig — comptime PEG parser engine
pub const parser_engine_zig = @embedFile("zig_templates/parser_engine.zig");

/// parse_abi.zig — C ABI types (standalone copy for grammar .so builds)
pub const parse_abi_zig = @embedFile("zig_templates/parse_abi.zig");

/// grammar_lib.zig — entry point for compiled grammar .so files
pub const grammar_lib_zig = @embedFile("zig_templates/grammar_lib.zig");

/// build.zig template — {s} is replaced with the library name
pub const build_zig_template =
    \\const std = @import("std");
    \\const builtin = @import("builtin");
    \\
    \\pub fn build(b: *std.Build) void {
    \\    const target = b.standardTargetOptions(.{});
    \\    const optimize = b.standardOptimizeOption(.{});
    \\
    \\    const lib_mod = b.createModule(.{
    \\        .root_source_file = b.path("src/grammar_lib.zig"),
    \\        .target = target,
    \\        .optimize = optimize,
    \\    });
    \\
    \\    const lib = b.addLibrary(.{
    \\        .name = "LIB_NAME_PLACEHOLDER",
    \\        .linkage = .dynamic,
    \\        .root_module = lib_mod,
    \\    });
    \\
    \\    lib.linkLibC();
    \\
    \\    const ext = if (builtin.os.tag == .windows) ".pyd" else ".so";
    \\    const install = b.addInstallArtifact(lib, .{
    \\        .dest_sub_path = "LIB_NAME_PLACEHOLDER" ++ ext,
    \\    });
    \\    b.getInstallStep().dependOn(&install.step);
    \\}
    \\
;

/// build.zig.zon template — LIB_NAME_PLACEHOLDER is replaced with the library name
pub const build_zig_zon_template =
    \\.{
    \\    .name = .LIB_NAME_PLACEHOLDER,
    \\    .version = "0.0.1",
    \\    .paths = .{ "build.zig", "build.zig.zon", "src" },
    \\}
    \\
;
