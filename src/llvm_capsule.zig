//! zgram's LLVM for other packages: the `zgram.llvm.v1` capsule
//! (`zgram.llvm_capsule()`), so a package generating native code (zrun)
//! uses the LLVM zgram already carries instead of a second copy.
//!
//! The interface is LLVM IR as text: a consumer writes a module's IR and
//! hands it over; zgram parses, verifies and optimizes it for this CPU and
//! adds it to its JIT, or emits an object file for another target. Text
//! keeps the boundary small and stable, and the IR readable when debugging.
//! A consumer checks `abi` before anything else, and writes the IR for this
//! LLVM version (`llvm_version`).

const std = @import("std");
const builtin = @import("builtin");
const jc = @import("jit_compiler.zig");
const LB = @import("llvm_builder.zig");
const c = LB.llvm;

pub const LLVM_ABI: u32 = 1;
pub const CAPSULE_NAME = "zgram.llvm.v1";

/// What the capsule points to. Every function is safe to call from any
/// thread (the JIT is shared and locked); none needs the GIL.
pub const LlvmView = extern struct {
    abi: u32 = LLVM_ABI,
    /// The LLVM version, "21.1.8"
    llvm_version: [*:0]const u8,
    /// Parse, verify, optimize (`opt_level` 0-3) and JIT-compile a module of
    /// IR text. Returns a handle for release(), or null with the error
    /// written to `err` (NUL-terminated, cut to `err_cap`).
    compile: *const fn (ir: [*]const u8, len: usize, opt_level: u32, err: [*]u8, err_cap: usize) callconv(.c) ?*anyopaque,
    /// The address of a function or global of a compiled module, by name
    /// (0 if there is none). Names are global to the process: a consumer
    /// keeps its own unique.
    lookup: *const fn (name: [*:0]const u8) callconv(.c) u64,
    /// Make native functions (a runtime's helpers) callable from compiled
    /// IR by name. 0, or -1 with the error in `err`.
    define: *const fn (names: [*]const [*:0]const u8, addrs: [*]const u64, n: usize, err: [*]u8, err_cap: usize) callconv(.c) i32,
    /// Free a compiled module's code (its addresses become invalid).
    release: *const fn (handle: ?*anyopaque) callconv(.c) void,
    /// Compile a module of IR text to an object file for a target (null:
    /// this process's), CPU and features (null: the host's, or generic for
    /// another target). Returns the bytes (free them with free_bytes) and
    /// their length, or null with the error in `err`.
    emit_object: *const fn (ir: [*]const u8, len: usize, opt_level: u32, triple: ?[*:0]const u8, cpu: ?[*:0]const u8, features: ?[*:0]const u8, out_len: *usize, err: [*]u8, err_cap: usize) callconv(.c) ?[*]u8,
    free_bytes: *const fn (bytes: ?[*]u8) callconv(.c) void,
    /// The JIT's target triple ("x86_64-unknown-linux-gnu") and data layout,
    /// for the IR's `target triple` / `target datalayout`; null if the JIT
    /// can't start (then compile() says why).
    triple: *const fn () callconv(.c) ?[*:0]const u8,
    data_layout: *const fn () callconv(.c) ?[*:0]const u8,
};

pub const view = LlvmView{
    .llvm_version = "21.1.8",
    .compile = &compile,
    .lookup = &lookup,
    .define = &define,
    .release = &release,
    .emit_object = &emitObject,
    .free_bytes = &freeBytes,
    .triple = &triple,
    .data_layout = &dataLayout,
};

fn setError(err: [*]u8, cap: usize, msg: []const u8) void {
    if (cap == 0) return;
    const n = @min(msg.len, cap - 1);
    @memcpy(err[0..n], msg[0..n]);
    err[n] = 0;
}

fn setLlvmError(err: [*]u8, cap: usize, e: c.LLVMErrorRef) void {
    const msg = c.LLVMGetErrorMessage(e);
    defer c.LLVMDisposeErrorMessage(msg);
    setError(err, cap, std.mem.span(msg));
}

/// The JIT, started if it wasn't (under the lock).
fn jit(err: [*]u8, cap: usize) ?c.LLVMOrcLLJITRef {
    jc.lockJit();
    defer jc.unlockJit();
    jc.initJit() catch {
        setError(err, cap, "LLVM's JIT couldn't start");
        return null;
    };
    return jc.global_jit;
}

/// Parse IR text into a new module of a new context.
fn parse(ctx: c.LLVMContextRef, ir: [*]const u8, len: usize, err: [*]u8, cap: usize) ?c.LLVMModuleRef {
    const buf = c.LLVMCreateMemoryBufferWithMemoryRangeCopy(ir, len, "zgram.llvm");
    var module: c.LLVMModuleRef = null;
    var msg: [*c]u8 = null;
    // (takes the buffer)
    if (c.LLVMParseIRInContext(ctx, buf, &module, &msg) != 0) {
        setError(err, cap, if (msg) |m| std.mem.span(m) else "the IR doesn't parse");
        if (msg) |m| c.LLVMDisposeMessage(m);
        return null;
    }
    if (c.LLVMVerifyModule(module, c.LLVMReturnStatusAction, &msg) != 0) {
        setError(err, cap, if (msg) |m| std.mem.span(m) else "the IR isn't valid");
        if (msg) |m| c.LLVMDisposeMessage(m);
        c.LLVMDisposeModule(module);
        return null;
    }
    if (msg) |m| c.LLVMDisposeMessage(m);
    return module;
}

fn pipeline(opt_level: u32) [*:0]const u8 {
    return switch (opt_level) {
        0 => "default<O0>",
        1 => "default<O1>",
        2 => "default<O2>",
        else => "default<O3>",
    };
}

fn compile(ir: [*]const u8, len: usize, opt_level: u32, err: [*]u8, err_cap: usize) callconv(.c) ?*anyopaque {
    const j = jit(err, err_cap) orelse return null;
    const ctx = c.LLVMContextCreate();
    const module = parse(ctx, ir, len, err, err_cap) orelse {
        c.LLVMContextDispose(ctx);
        return null;
    };
    // (outside the lock: modules of their own contexts optimize in parallel)
    jc.optimizeModuleWith(j, module, pipeline(opt_level)) catch {
        c.LLVMDisposeModule(module);
        c.LLVMContextDispose(ctx);
        setError(err, err_cap, "LLVM's optimizer failed");
        return null;
    };
    jc.lockJit();
    defer jc.unlockJit();
    const dylib = c.LLVMOrcLLJITGetMainJITDylib(j);
    const tracker = c.LLVMOrcJITDylibCreateResourceTracker(dylib);
    const ts_ctx = c.LLVMOrcCreateNewThreadSafeContextFromLLVMContext(ctx);
    const ts_module = c.LLVMOrcCreateNewThreadSafeModule(module, ts_ctx);
    c.LLVMOrcDisposeThreadSafeContext(ts_ctx);
    if (c.LLVMOrcLLJITAddLLVMIRModuleWithRT(j, tracker, ts_module)) |e| {
        setLlvmError(err, err_cap, e);
        c.LLVMOrcReleaseResourceTracker(tracker);
        return null;
    }
    return @ptrCast(tracker);
}

fn lookup(name: [*:0]const u8) callconv(.c) u64 {
    var buf: [8]u8 = undefined;
    const j = jit(&buf, buf.len) orelse return 0;
    jc.lockJit();
    defer jc.unlockJit();
    var addr: c.LLVMOrcExecutorAddress = 0;
    if (c.LLVMOrcLLJITLookup(j, &addr, name)) |e| {
        c.LLVMConsumeError(e);
        return 0;
    }
    return addr;
}

fn define(names: [*]const [*:0]const u8, addrs: [*]const u64, n: usize, err: [*]u8, err_cap: usize) callconv(.c) i32 {
    const j = jit(err, err_cap) orelse return -1;
    jc.lockJit();
    defer jc.unlockJit();
    const es = c.LLVMOrcLLJITGetExecutionSession(j);
    const dylib = c.LLVMOrcLLJITGetMainJITDylib(j);
    const flags = c.LLVMJITSymbolFlags{ .GenericFlags = c.LLVMJITSymbolGenericFlagsExported | c.LLVMJITSymbolGenericFlagsCallable, .TargetFlags = 0 };
    const pairs = std.heap.c_allocator.alloc(c.LLVMOrcCSymbolMapPair, n) catch {
        setError(err, err_cap, "out of memory");
        return -1;
    };
    defer std.heap.c_allocator.free(pairs);
    for (pairs, 0..) |*p, i| {
        p.* = .{ .Name = c.LLVMOrcExecutionSessionIntern(es, names[i]), .Sym = .{ .Address = addrs[i], .Flags = flags } };
    }
    const mu = c.LLVMOrcAbsoluteSymbols(pairs.ptr, n);
    if (c.LLVMOrcJITDylibDefine(dylib, mu)) |e| {
        setLlvmError(err, err_cap, e);
        return -1;
    }
    return 0;
}

fn release(handle: ?*anyopaque) callconv(.c) void {
    const tracker: c.LLVMOrcResourceTrackerRef = @ptrCast(handle orelse return);
    jc.lockJit();
    defer jc.unlockJit();
    if (c.LLVMOrcResourceTrackerRemove(tracker)) |e| c.LLVMConsumeError(e);
    c.LLVMOrcReleaseResourceTracker(tracker);
}

fn emitObject(ir: [*]const u8, len: usize, opt_level: u32, triple_opt: ?[*:0]const u8, cpu_opt: ?[*:0]const u8, features_opt: ?[*:0]const u8, out_len: *usize, err: [*]u8, err_cap: usize) callconv(.c) ?[*]u8 {
    const j = jit(err, err_cap) orelse return null;
    const host_triple = c.LLVMOrcLLJITGetTripleString(j);
    const t = triple_opt orelse host_triple;
    const is_host = triple_opt == null;
    const host_cpu = c.LLVMGetHostCPUName();
    defer c.LLVMDisposeMessage(host_cpu);
    const host_features = c.LLVMGetHostCPUFeatures();
    defer c.LLVMDisposeMessage(host_features);
    const cpu: [*:0]const u8 = cpu_opt orelse if (is_host) host_cpu else "generic";
    const features: [*:0]const u8 = features_opt orelse if (is_host) host_features else "";

    var target: c.LLVMTargetRef = null;
    var msg: [*c]u8 = null;
    if (c.LLVMGetTargetFromTriple(t, &target, &msg) != 0) {
        setError(err, err_cap, if (msg) |m| std.mem.span(m) else "unknown target");
        if (msg) |m| c.LLVMDisposeMessage(m);
        return null;
    }
    const level: c.LLVMCodeGenOptLevel = switch (opt_level) {
        0 => c.LLVMCodeGenLevelNone,
        1 => c.LLVMCodeGenLevelLess,
        2 => c.LLVMCodeGenLevelDefault,
        else => c.LLVMCodeGenLevelAggressive,
    };
    const tm = c.LLVMCreateTargetMachine(target, t, cpu, features, level, c.LLVMRelocPIC, c.LLVMCodeModelDefault);
    defer c.LLVMDisposeTargetMachine(tm);

    const ctx = c.LLVMContextCreate();
    defer c.LLVMContextDispose(ctx);
    const module = parse(ctx, ir, len, err, err_cap) orelse return null;
    defer c.LLVMDisposeModule(module);
    c.LLVMSetTarget(module, t);
    const layout = c.LLVMCreateTargetDataLayout(tm);
    defer c.LLVMDisposeTargetData(layout);
    c.LLVMSetModuleDataLayout(module, layout);
    const opts = c.LLVMCreatePassBuilderOptions();
    defer c.LLVMDisposePassBuilderOptions(opts);
    if (c.LLVMRunPasses(module, pipeline(opt_level), tm, opts)) |e| {
        setLlvmError(err, err_cap, e);
        return null;
    }

    var buf: c.LLVMMemoryBufferRef = null;
    if (c.LLVMTargetMachineEmitToMemoryBuffer(tm, module, c.LLVMObjectFile, &msg, &buf) != 0) {
        setError(err, err_cap, if (msg) |m| std.mem.span(m) else "emitting the object failed");
        if (msg) |m| c.LLVMDisposeMessage(m);
        return null;
    }
    defer c.LLVMDisposeMemoryBuffer(buf);
    const size = c.LLVMGetBufferSize(buf);
    const start: [*]const u8 = @ptrCast(c.LLVMGetBufferStart(buf));
    const out = std.heap.c_allocator.alloc(u8, size) catch {
        setError(err, err_cap, "out of memory");
        return null;
    };
    @memcpy(out, start[0..size]);
    out_len.* = size;
    return out.ptr;
}

fn freeBytes(bytes: ?[*]u8) callconv(.c) void {
    // (allocated by c_allocator, which is malloc: free takes the pointer)
    std.c.free(bytes);
}

fn triple() callconv(.c) ?[*:0]const u8 {
    var buf: [8]u8 = undefined;
    const j = jit(&buf, buf.len) orelse return null;
    return c.LLVMOrcLLJITGetTripleString(j);
}

fn dataLayout() callconv(.c) ?[*:0]const u8 {
    var buf: [8]u8 = undefined;
    const j = jit(&buf, buf.len) orelse return null;
    return c.LLVMOrcLLJITGetDataLayoutStr(j);
}
