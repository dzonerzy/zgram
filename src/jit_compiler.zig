//! JIT compiler: takes an in-memory LLVM module and produces a callable
//! function pointer via LLVM's ORC LLJIT.
//!
//! Uses LLVM C API for JIT compilation. The module is built directly
//! by jit_codegen.zig using the LLVM C API (no bitcode step).

const std = @import("std");
const builtin = @import("builtin");
const abi = @import("parse_abi.zig");
const LB = @import("llvm_builder.zig");

// Use the same LLVM C bindings as llvm_builder to avoid opaque type mismatches
const c = LB.llvm;

// Extern declarations for JIT helper functions (defined in jit_helpers.zig via export)
extern fn zgram_reserve_node(output: *abi.ParseOutput) callconv(.c) i32;
extern fn zgram_fill_node(output: *abi.ParseOutput, idx: u32, rule_id: u16, text_start: u32, text_end: u32, subtree_size: u32, child_count: u16) callconv(.c) void;
extern fn zgram_set_error_trailing(output: *abi.ParseOutput, input_ptr: [*]const u8, input_len: usize, pos: usize) callconv(.c) void;
extern fn zgram_set_error_at_hwm(output: *abi.ParseOutput, input_ptr: [*]const u8, input_len: usize) callconv(.c) void;
extern fn zgram_ensure_capacity(output: *abi.ParseOutput, needed: u32) callconv(.c) i32;
extern fn zgram_memo_lookup(output: *abi.ParseOutput, rule_id: u32, pos: u64) callconv(.c) i64;
extern fn zgram_tag_field(output: *abi.ParseOutput, from: u32, field: u32) callconv(.c) void;
extern fn zgram_fold(output: *abi.ParseOutput, first: u32, rule_id: u32, kind: u32, k: u32, start: u32, end: u32) callconv(.c) i32;
extern fn zgram_memo_store(output: *abi.ParseOutput, rule_id: u32, pos: u64, result: i64, node_start: u32) callconv(.c) void;
extern fn zgram_recover_error(output: *abi.ParseOutput, start: u64, reach: u64) callconv(.c) i64;
extern fn zgram_recover_begin(output: *abi.ParseOutput, input_ptr: [*]const u8, start: u64, err: u64) callconv(.c) void;
extern fn zgram_recover_step(output: *abi.ParseOutput, input_ptr: [*]const u8, input_len: u64) callconv(.c) i64;
extern fn zgram_error_node(output: *abi.ParseOutput, start: u64, end: u64, rule: u32) callconv(.c) i32;

// X86 target init (macro-generated in Target.h, must declare manually)
extern fn LLVMInitializeX86TargetInfo() void;
extern fn LLVMInitializeX86Target() void;
extern fn LLVMInitializeX86TargetMC() void;
extern fn LLVMInitializeX86AsmPrinter() void;
extern fn LLVMInitializeX86AsmParser() void;

/// Errors from JIT compilation
pub const JitError = error{
    LLVMError,
    JitNotInitialized,
    SymbolNotFound,
};

/// Global LLJIT instance — created once, reused across all grammar compilations.
var global_jit: ?c.LLVMOrcLLJITRef = null;
var jit_initialized: bool = false;
/// Counter for creating unique entry point names per grammar compilation.
var dylib_counter: u64 = 0;

/// Serializes JIT setup, compilation and release, so grammars can be
/// compiled from several threads (with the GIL released, or from async
/// tasks). A spinning lock that yields is enough: compiles take milliseconds
/// and rarely overlap.
var jit_lock: std.atomic.Value(bool) = .init(false);

fn lockJit() void {
    while (jit_lock.cmpxchgWeak(false, true, .acquire, .monotonic) != null) {
        std.Thread.yield() catch {};
    }
}

fn unlockJit() void {
    jit_lock.store(false, .release);
}

/// Initialize the LLVM JIT subsystem. Called once on first compile.
fn initJit() JitError!void {
    if (jit_initialized) return;

    // Initialize native target
    LLVMInitializeX86TargetInfo();
    LLVMInitializeX86Target();
    LLVMInitializeX86TargetMC();
    LLVMInitializeX86AsmPrinter();
    LLVMInitializeX86AsmParser();

    // Create LLJIT
    var jit: c.LLVMOrcLLJITRef = null;
    try handleError(c.LLVMOrcCreateLLJIT(&jit, null));

    // Register helper symbols once in the main dylib
    const dylib = c.LLVMOrcLLJITGetMainJITDylib(jit);
    try registerHelperSymbols(jit, dylib);

    global_jit = jit;
    jit_initialized = true;
}

/// Register zgram helper functions as absolute symbols in a JITDylib.
/// Must be called before adding a module so LLJIT can resolve helper calls.
fn registerHelperSymbols(jit: c.LLVMOrcLLJITRef, dylib: c.LLVMOrcJITDylibRef) JitError!void {
    const es = c.LLVMOrcLLJITGetExecutionSession(jit);
    const exported_flags = c.LLVMJITSymbolFlags{ .GenericFlags = c.LLVMJITSymbolGenericFlagsExported | c.LLVMJITSymbolGenericFlagsCallable, .TargetFlags = 0 };

    // On Windows, functions with large stack frames call the stack probe
    // ___chkstk_ms; it's in Zig's compiler runtime, linked into this module.
    const is_windows = builtin.os.tag == .windows;
    const chkstk: usize = if (is_windows)
        @intFromPtr(@extern(*const fn () callconv(.naked) void, .{ .name = "___chkstk_ms" }))
    else
        0;

    var syms: [14]c.LLVMOrcCSymbolMapPair = .{
        .{ .Name = c.LLVMOrcExecutionSessionIntern(es, "zgram_recover_error"), .Sym = .{ .Address = @intFromPtr(&zgram_recover_error), .Flags = exported_flags } },
        .{ .Name = c.LLVMOrcExecutionSessionIntern(es, "zgram_recover_begin"), .Sym = .{ .Address = @intFromPtr(&zgram_recover_begin), .Flags = exported_flags } },
        .{ .Name = c.LLVMOrcExecutionSessionIntern(es, "zgram_recover_step"), .Sym = .{ .Address = @intFromPtr(&zgram_recover_step), .Flags = exported_flags } },
        .{ .Name = c.LLVMOrcExecutionSessionIntern(es, "zgram_error_node"), .Sym = .{ .Address = @intFromPtr(&zgram_error_node), .Flags = exported_flags } },
        .{ .Name = c.LLVMOrcExecutionSessionIntern(es, "zgram_reserve_node"), .Sym = .{ .Address = @intFromPtr(&zgram_reserve_node), .Flags = exported_flags } },
        .{ .Name = c.LLVMOrcExecutionSessionIntern(es, "zgram_fill_node"), .Sym = .{ .Address = @intFromPtr(&zgram_fill_node), .Flags = exported_flags } },
        .{ .Name = c.LLVMOrcExecutionSessionIntern(es, "zgram_set_error_trailing"), .Sym = .{ .Address = @intFromPtr(&zgram_set_error_trailing), .Flags = exported_flags } },
        .{ .Name = c.LLVMOrcExecutionSessionIntern(es, "zgram_set_error_at_hwm"), .Sym = .{ .Address = @intFromPtr(&zgram_set_error_at_hwm), .Flags = exported_flags } },
        .{ .Name = c.LLVMOrcExecutionSessionIntern(es, "zgram_ensure_capacity"), .Sym = .{ .Address = @intFromPtr(&zgram_ensure_capacity), .Flags = exported_flags } },
        .{ .Name = c.LLVMOrcExecutionSessionIntern(es, "zgram_memo_lookup"), .Sym = .{ .Address = @intFromPtr(&zgram_memo_lookup), .Flags = exported_flags } },
        .{ .Name = c.LLVMOrcExecutionSessionIntern(es, "zgram_memo_store"), .Sym = .{ .Address = @intFromPtr(&zgram_memo_store), .Flags = exported_flags } },
        .{ .Name = c.LLVMOrcExecutionSessionIntern(es, "zgram_tag_field"), .Sym = .{ .Address = @intFromPtr(&zgram_tag_field), .Flags = exported_flags } },
        .{ .Name = c.LLVMOrcExecutionSessionIntern(es, "zgram_fold"), .Sym = .{ .Address = @intFromPtr(&zgram_fold), .Flags = exported_flags } },
        .{ .Name = c.LLVMOrcExecutionSessionIntern(es, "___chkstk_ms"), .Sym = .{ .Address = chkstk, .Flags = exported_flags } },
    };

    const mu = c.LLVMOrcAbsoluteSymbols(&syms, if (is_windows) syms.len else syms.len - 1);
    try handleError(c.LLVMOrcJITDylibDefine(dylib, mu));
}

/// Run the full LLVM optimization pipeline on a module.
/// Uses the new pass manager with O3 + vectorization + loop opts targeting the host CPU.
fn optimizeModule(jit: c.LLVMOrcLLJITRef, module: c.LLVMModuleRef) JitError!void {
    // Target the JIT's own triple (the process it runs in), with the host
    // CPU and its features. LLVMGetDefaultTargetTriple
    // is fixed when LLVM is built, and the bundled Windows LLVM was built on
    // Linux: its default triple is an ELF one, which the Windows JIT rejects.
    const triple = c.LLVMOrcLLJITGetTripleString(jit);
    const cpu = c.LLVMGetHostCPUName();
    defer c.LLVMDisposeMessage(cpu);
    const features = c.LLVMGetHostCPUFeatures();
    defer c.LLVMDisposeMessage(features);

    // Set module target so passes know the architecture
    c.LLVMSetTarget(module, triple);

    // Create target machine for this host
    var target: c.LLVMTargetRef = null;
    var err_msg: [*c]u8 = null;
    if (c.LLVMGetTargetFromTriple(triple, &target, &err_msg) != 0) {
        if (err_msg) |m| c.LLVMDisposeMessage(m);
        return JitError.LLVMError;
    }

    const tm = c.LLVMCreateTargetMachine(
        target,
        triple,
        cpu,
        features,
        c.LLVMCodeGenLevelAggressive,
        c.LLVMRelocDefault,
        c.LLVMCodeModelJITDefault,
    );
    defer c.LLVMDisposeTargetMachine(tm);

    // Give the optimizer the real data layout (sizes, alignments, native widths)
    const layout = c.LLVMCreateTargetDataLayout(tm);
    defer c.LLVMDisposeTargetData(layout);
    c.LLVMSetModuleDataLayout(module, layout);

    // Configure pass builder options — enable everything
    const opts = c.LLVMCreatePassBuilderOptions();
    defer c.LLVMDisposePassBuilderOptions(opts);
    c.LLVMPassBuilderOptionsSetLoopInterleaving(opts, 1);
    c.LLVMPassBuilderOptionsSetLoopVectorization(opts, 1);
    c.LLVMPassBuilderOptionsSetSLPVectorization(opts, 1);
    c.LLVMPassBuilderOptionsSetLoopUnrolling(opts, 1);
    c.LLVMPassBuilderOptionsSetMergeFunctions(opts, 1);
    c.LLVMPassBuilderOptionsSetInlinerThreshold(opts, 500);

    // Run the full O3 pipeline
    try handleError(c.LLVMRunPasses(module, "default<O3>", tm, opts));
}

/// Opaque handle for tracking JIT resources associated with a compiled grammar.
/// Pass to `releaseGrammar()` to free JIT code when the grammar is no longer needed.
pub const ResourceHandle = c.LLVMOrcResourceTrackerRef;

/// Result of JIT compilation: a callable parse function + a resource handle for cleanup.
pub const JitResult = struct {
    parse_fn: abi.ParseFn,
    resource: ResourceHandle,
};

/// JIT-compile an in-memory LLVM module into a callable parse function pointer.
///
/// Takes ownership of both the module and context (they are consumed by LLJIT).
/// Returns a JitResult with the function pointer and a ResourceHandle that must
/// be passed to `releaseGrammar()` when the grammar is no longer needed.
pub fn jitCompile(module: c.LLVMModuleRef, ctx: c.LLVMContextRef) JitError!JitResult {
    // Ensure JIT (and the target registry the optimizer needs) is initialized
    const jit = blk: {
        lockJit();
        defer unlockJit();
        try initJit();
        break :blk global_jit orelse return JitError.JitNotInitialized;
    };

    // Run the O3 pipeline. The module and context belong to this call, so
    // concurrent compiles optimize in parallel.
    try optimizeModule(jit, module);

    lockJit();
    defer unlockJit();

    // Give the entry point a unique name so multiple compiled parsers
    // can coexist without symbol conflicts.
    const id = dylib_counter;
    dylib_counter += 1;

    var fn_name_buf: [48]u8 = undefined;
    const fn_name = std.fmt.bufPrint(&fn_name_buf, "zgram_parse_{d}\x00", .{id}) catch return JitError.LLVMError;
    const fn_name_z: [*:0]const u8 = @ptrCast(fn_name.ptr);

    // Rename zgram_parse → zgram_parse_N in the module
    const parse_func = c.LLVMGetNamedFunction(module, "zgram_parse");
    if (parse_func) |f| {
        c.LLVMSetValueName(f, fn_name_z);
    } else {
        return JitError.SymbolNotFound;
    }

    // Use the main JITDylib with a ResourceTracker so we can free this
    // grammar's JIT code independently when it's no longer needed.
    const dylib = c.LLVMOrcLLJITGetMainJITDylib(jit);
    const rt = c.LLVMOrcJITDylibCreateResourceTracker(dylib);

    // Wrap context in ThreadSafeContext (takes ownership of ctx)
    const ts_ctx = c.LLVMOrcCreateNewThreadSafeContextFromLLVMContext(ctx);

    // Wrap module in ThreadSafeModule (takes ownership of module and ts_ctx)
    const ts_module = c.LLVMOrcCreateNewThreadSafeModule(module, ts_ctx);

    // Add module to JIT, tracked by the ResourceTracker
    const add_err = c.LLVMOrcLLJITAddLLVMIRModuleWithRT(jit, rt, ts_module);
    if (add_err) |e| {
        c.LLVMOrcReleaseResourceTracker(rt);
        const msg = c.LLVMGetErrorMessage(e);
        defer c.LLVMDisposeErrorMessage(msg);
        std.log.err("LLVM JIT error: {s}", .{msg});
        return JitError.LLVMError;
    }

    // Look up the uniquely-named function
    var fn_addr: c.LLVMOrcExecutorAddress = 0;
    try handleError(c.LLVMOrcLLJITLookup(jit, &fn_addr, fn_name_z));

    if (fn_addr == 0) {
        c.LLVMOrcReleaseResourceTracker(rt);
        return JitError.SymbolNotFound;
    }

    return .{
        .parse_fn = @ptrFromInt(fn_addr),
        .resource = rt,
    };
}

/// Release all JIT-compiled code and resources associated with a grammar.
/// After this call, the parse function pointer is invalid and must not be used.
pub fn releaseGrammar(resource: ResourceHandle) void {
    lockJit();
    defer unlockJit();
    if (resource) |rt| {
        // Remove all JIT code tracked by this ResourceTracker
        const err = c.LLVMOrcResourceTrackerRemove(rt);
        if (err) |e| {
            const msg = c.LLVMGetErrorMessage(e);
            defer c.LLVMDisposeErrorMessage(msg);
            std.log.err("LLVM JIT cleanup error: {s}", .{msg});
        }
        // Release the tracker ref-count
        c.LLVMOrcReleaseResourceTracker(rt);
    }
}

/// Convert an LLVMErrorRef into a Zig error.
fn handleError(err: c.LLVMErrorRef) JitError!void {
    if (err) |e| {
        const msg = c.LLVMGetErrorMessage(e);
        defer c.LLVMDisposeErrorMessage(msg);
        std.log.err("LLVM JIT error: {s}", .{msg});
        return JitError.LLVMError;
    }
}
