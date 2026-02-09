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

    // Select the correct LLVM dependency based on target
    const target_os = target.result.os.tag;
    const target_arch = target.result.cpu.arch;

    const llvm_dep_name: []const u8 = switch (target_os) {
        .windows => switch (target_arch) {
            .x86_64 => "llvm_x86_64_windows",
            .aarch64 => "llvm_aarch64_windows",
            else => @panic("unsupported architecture for LLVM libs"),
        },
        .macos => "llvm_aarch64_macos",
        .linux => switch (target_arch) {
            .x86_64 => "llvm_x86_64_linux",
            .aarch64 => "llvm_aarch64_linux",
            else => @panic("unsupported architecture for LLVM libs"),
        },
        else => @panic("unsupported OS for LLVM libs"),
    };

    const llvm_dep = b.lazyDependency(llvm_dep_name, .{}) orelse return;
    const llvm_lib_path = llvm_dep.path("lib");
    const llvm_include_path = llvm_dep.path("include");

    // LLVM C API headers
    user_lib_mod.addIncludePath(llvm_include_path);

    // LLVM static libs (63 total)
    const llvm_libs = [_][]const u8{
        // JIT
        "LLVMOrcJIT",                "LLVMJITLink",                "LLVMExecutionEngine",
        "LLVMRuntimeDyld",           "LLVMOrcTargetProcess",       "LLVMOrcShared",
        // Passes / Optimization
        "LLVMPasses",                "LLVMCoroutines",             "LLVMipo",
        "LLVMInstrumentation",       "LLVMVectorize",              "LLVMLinker",
        "LLVMScalarOpts",            "LLVMInstCombine",            "LLVMObjCARCOpts",
        "LLVMAggressiveInstCombine", "LLVMTransformUtils",         "LLVMCFGuard",
        // CodeGen
        "LLVMCodeGen",               "LLVMCodeGenTypes",           "LLVMSelectionDAG",
        "LLVMGlobalISel",            "LLVMAsmPrinter",             "LLVMCGData",
        // X86 Backend
        "LLVMX86CodeGen",            "LLVMX86Desc",                "LLVMX86Info",
        "LLVMX86AsmParser",          "LLVMX86Disassembler",        "LLVMX86TargetMCA",
        // Core / IR
        "LLVMCore",                  "LLVMBitReader",              "LLVMBitWriter",
        "LLVMIRReader",              "LLVMIRPrinter",              "LLVMAsmParser",
        "LLVMBitstreamReader",
        // Target / MC
              "LLVMTarget",                 "LLVMTargetParser",
        "LLVMMC",                    "LLVMMCParser",               "LLVMMCDisassembler",
        "LLVMMCA",
        // Analysis / Support
                          "LLVMAnalysis",               "LLVMProfileData",
        "LLVMObject",                "LLVMTextAPI",                "LLVMBinaryFormat",
        "LLVMRemarks",               "LLVMSupport",                "LLVMDemangle",
        // Debug Info
        "LLVMDebugInfoDWARF",        "LLVMDebugInfoDWARFLowLevel", "LLVMDebugInfoPDB",
        "LLVMDebugInfoMSF",          "LLVMDebugInfoBTF",           "LLVMDebugInfoCodeView",
        "LLVMSymbolize",
        // Frontend
                    "LLVMFrontendOpenMP",         "LLVMFrontendOffloading",
        "LLVMFrontendAtomic",
        // Misc
               "LLVMHipStdPar",              "LLVMWindowsDriver",
        "LLVMOption",                "LLVMSandboxIR",              "LLVMObjectYAML",
    };

    for (llvm_libs) |lib_name| {
        user_lib_mod.addObjectFile(llvm_lib_path.path(b, b.fmt("lib{s}.a", .{lib_name})));
    }

    // Build the Python extension as a dynamic library
    const lib = b.addLibrary(.{
        .name = "zgram",
        .linkage = .dynamic,
        .root_module = user_lib_mod,
    });

    // Export all symbols so LLJIT's dlsym() can find __register_frame etc.
    lib.rdynamic = true;

    // Link libc (required for Python C API)
    lib.linkLibC();
    lib.linkLibCpp();

    // Platform-specific link dependencies
    switch (target_os) {
        .linux => {
            user_lib_mod.linkSystemLibrary("rt", .{});
            user_lib_mod.linkSystemLibrary("dl", .{});
            user_lib_mod.linkSystemLibrary("m", .{});
            user_lib_mod.linkSystemLibrary("pthread", .{});
        },
        .windows => {
            user_lib_mod.linkSystemLibrary("psapi", .{});
            user_lib_mod.linkSystemLibrary("ole32", .{});
            user_lib_mod.linkSystemLibrary("oleaut32", .{});
            user_lib_mod.linkSystemLibrary("advapi32", .{});
            user_lib_mod.linkSystemLibrary("shell32", .{});
            user_lib_mod.linkSystemLibrary("shlwapi", .{});
            user_lib_mod.linkSystemLibrary("uuid", .{});
            user_lib_mod.linkSystemLibrary("user32", .{});
        },
        else => {},
    }

    // Determine extension based on target OS (.pyd for Windows, .so otherwise)
    const ext = if (builtin.os.tag == .windows) ".pyd" else ".so";

    // Install the shared library
    const install = b.addInstallArtifact(lib, .{
        .dest_sub_path = "zgram" ++ ext,
    });
    b.getInstallStep().dependOn(&install.step);

    // ── Benchmark executable (no Python / PyOZ dependency) ──

    // Core module: re-exports the compiler pipeline without Python/PyOZ
    const core_mod = b.createModule(.{
        .root_source_file = b.path("src/zgram_core.zig"),
        .target = target,
        .optimize = optimize,
    });
    core_mod.addIncludePath(llvm_include_path);
    for (llvm_libs) |lib_name| {
        core_mod.addObjectFile(llvm_lib_path.path(b, b.fmt("lib{s}.a", .{lib_name})));
    }

    const bench_mod = b.createModule(.{
        .root_source_file = b.path("benchmark/zgram/main.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "zgram_core", .module = core_mod },
        },
    });

    const bench_exe = b.addExecutable(.{
        .name = "zgram_bench",
        .root_module = bench_mod,
    });
    bench_exe.rdynamic = true;
    bench_exe.linkLibC();
    bench_exe.linkLibCpp();

    switch (target_os) {
        .linux => {
            bench_mod.linkSystemLibrary("rt", .{});
            bench_mod.linkSystemLibrary("dl", .{});
            bench_mod.linkSystemLibrary("m", .{});
            bench_mod.linkSystemLibrary("pthread", .{});
        },
        .windows => {
            bench_mod.linkSystemLibrary("psapi", .{});
            bench_mod.linkSystemLibrary("ole32", .{});
            bench_mod.linkSystemLibrary("oleaut32", .{});
            bench_mod.linkSystemLibrary("advapi32", .{});
            bench_mod.linkSystemLibrary("shell32", .{});
            bench_mod.linkSystemLibrary("shlwapi", .{});
            bench_mod.linkSystemLibrary("uuid", .{});
            bench_mod.linkSystemLibrary("user32", .{});
        },
        else => {},
    }

    const bench_install = b.addInstallArtifact(bench_exe, .{});
    const bench_step = b.step("bench", "Build the native benchmark");
    bench_step.dependOn(&bench_install.step);
}
