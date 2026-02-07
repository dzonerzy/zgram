//! Build script for zgram
//!
//! You can use this directly with `zig build`, or use `pyoz build` for
//! automatic Python configuration detection.

const std = @import("std");
const builtin = @import("builtin");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Strip option (can be set via -Dstrip=true or from pyoz CLI)
    const strip = b.option(bool, "strip", "Strip debug symbols from the binary") orelse false;

    // Get PyOZ dependency
    const pyoz_dep = b.dependency("PyOZ", .{
        .target = target,
        .optimize = optimize,
    });

    // Create the user's lib module (shared between library and stub generator)
    const user_lib_mod = b.createModule(.{
        .root_source_file = b.path("src/lib.zig"),
        .target = target,
        .optimize = optimize,
        .strip = strip,
        .imports = &.{
            .{ .name = "PyOZ", .module = pyoz_dep.module("PyOZ") },
        },
    });

    // Build the Python extension as a dynamic library
    const lib = b.addLibrary(.{
        .name = "zgram",
        .linkage = .dynamic,
        .root_module = user_lib_mod,
    });

    // Link libc (required for Python C API)
    lib.linkLibC();

    // Determine extension based on target OS (.pyd for Windows, .so otherwise)
    const ext = if (builtin.os.tag == .windows) ".pyd" else ".so";

    // Install the shared library
    const install = b.addInstallArtifact(lib, .{
        .dest_sub_path = "zgram" ++ ext,
    });
    b.getInstallStep().dependOn(&install.step);
}
