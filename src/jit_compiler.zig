//! JIT compiler: takes an in-memory LLVM module and produces a callable
//! function pointer via LLVM's ORC LLJIT.
//!
//! Uses LLVM C API for JIT compilation. The module is built directly
//! by jit_codegen.zig using the LLVM C API (no bitcode step).

const std = @import("std");
const abi = @import("parse_abi.zig");
const LB = @import("llvm_builder.zig");

// Use the same LLVM C bindings as llvm_builder to avoid opaque type mismatches
const c = LB.llvm;

// Extern declarations for JIT helper functions (defined in jit_helpers.zig via export)
extern fn zgram_reserve_node(output: *abi.ParseOutput) callconv(.c) i32;
extern fn zgram_fill_node(output: *abi.ParseOutput, idx: u32, rule_id: u16, text_start: u32, text_end: u32, subtree_size: u32, child_count: u16) callconv(.c) void;
extern fn zgram_set_error(output: *abi.ParseOutput, input_ptr: [*]const u8, input_len: usize, pos: usize, msg_ptr: [*]const u8, msg_len: usize) callconv(.c) void;
extern fn zgram_set_rule_name(output: *abi.ParseOutput, rule_id: u16, name_ptr: [*]const u8, name_len: u8) callconv(.c) void;
extern fn zgram_ensure_capacity(output: *abi.ParseOutput, needed: u32) callconv(.c) i32;

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
/// Counter for creating unique JITDylib names per grammar compilation.
var dylib_counter: u64 = 0;

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

    var syms: [5]c.LLVMOrcCSymbolMapPair = .{
        .{ .Name = c.LLVMOrcExecutionSessionIntern(es, "zgram_reserve_node"), .Sym = .{ .Address = @intFromPtr(&zgram_reserve_node), .Flags = exported_flags } },
        .{ .Name = c.LLVMOrcExecutionSessionIntern(es, "zgram_fill_node"), .Sym = .{ .Address = @intFromPtr(&zgram_fill_node), .Flags = exported_flags } },
        .{ .Name = c.LLVMOrcExecutionSessionIntern(es, "zgram_set_error"), .Sym = .{ .Address = @intFromPtr(&zgram_set_error), .Flags = exported_flags } },
        .{ .Name = c.LLVMOrcExecutionSessionIntern(es, "zgram_set_rule_name"), .Sym = .{ .Address = @intFromPtr(&zgram_set_rule_name), .Flags = exported_flags } },
        .{ .Name = c.LLVMOrcExecutionSessionIntern(es, "zgram_ensure_capacity"), .Sym = .{ .Address = @intFromPtr(&zgram_ensure_capacity), .Flags = exported_flags } },
    };

    const mu = c.LLVMOrcAbsoluteSymbols(&syms, syms.len);
    try handleError(c.LLVMOrcJITDylibDefine(dylib, mu));
}

/// Run the full LLVM optimization pipeline on a module.
/// Uses the new pass manager with O3 + vectorization + loop opts targeting the host CPU.
fn optimizeModule(module: c.LLVMModuleRef) JitError!void {
    // Get host target triple, CPU, and features for optimal codegen
    const triple = c.LLVMGetDefaultTargetTriple();
    defer c.LLVMDisposeMessage(triple);
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

/// JIT-compile an in-memory LLVM module into a callable parse function pointer.
///
/// Takes ownership of both the module and context (they are consumed by LLJIT).
/// Can be called multiple times — clears the previous compilation each time.
pub fn jitCompile(module: c.LLVMModuleRef, ctx: c.LLVMContextRef) JitError!abi.ParseFn {
    // Ensure JIT is initialized
    try initJit();
    const jit = global_jit orelse return JitError.JitNotInitialized;

    // Run full O3 optimization pipeline before JIT compilation
    try optimizeModule(module);

    // Give the entry point a unique name so multiple compiled parsers
    // can coexist in separate JITDylibs without symbol conflicts.
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

    // Use the main JITDylib — unique function names prevent conflicts
    const dylib = c.LLVMOrcLLJITGetMainJITDylib(jit);

    // Wrap context in ThreadSafeContext (takes ownership of ctx)
    const ts_ctx = c.LLVMOrcCreateNewThreadSafeContextFromLLVMContext(ctx);

    // Wrap module in ThreadSafeModule (takes ownership of module and ts_ctx)
    const ts_module = c.LLVMOrcCreateNewThreadSafeModule(module, ts_ctx);

    // Add module to JIT
    try handleError(c.LLVMOrcLLJITAddLLVMIRModule(jit, dylib, ts_module));

    // Look up the uniquely-named function
    var fn_addr: c.LLVMOrcExecutorAddress = 0;
    try handleError(c.LLVMOrcLLJITLookup(jit, &fn_addr, fn_name_z));

    if (fn_addr == 0) return JitError.SymbolNotFound;

    return @ptrFromInt(fn_addr);
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
