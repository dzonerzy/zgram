//! zgram's LLVM for other packages: the `zgram.llvm.v1` capsule
//! (`zgram.llvm_capsule()`), so a package generating native code (zrun)
//! uses the LLVM zgram already carries instead of a second copy.
//!
//! The interface is LLVM's own C API: a consumer looks its functions up by
//! name (`function("LLVMBuildAdd")`), builds a module in memory with them as
//! zgram's code generator does, and hands the module over: zgram verifies and
//! optimizes it for this CPU and adds it to its JIT, or emits an object file
//! for another target. A consumer checks `abi` first, and uses the C API of
//! this LLVM version (`llvm_version`).

const std = @import("std");
const jc = @import("jit_compiler.zig");
const LB = @import("llvm_builder.zig");
const c = LB.llvm;

pub const LLVM_ABI: u32 = 2;
pub const CAPSULE_NAME = "zgram.llvm.v1";

/// What the capsule points to. The JIT functions are safe to call from any
/// thread (the JIT is shared and locked); none needs the GIL. The C API's
/// functions follow LLVM's rules: a context and what's made in it are used
/// by one thread at a time.
pub const LlvmView = extern struct {
    abi: u32 = LLVM_ABI,
    /// The LLVM version, "21.1.8"
    llvm_version: [*:0]const u8,
    /// A function of LLVM's C API by name ("LLVMBuildAdd"), or null if the
    /// capsule doesn't export it (`api_names` lists those it does)
    function: *const fn (name: [*:0]const u8) callconv(.c) ?*const anyopaque,
    /// Verify, optimize (`opt_level` 0-3) and JIT-compile a module. Takes the
    /// module and its context (made for it alone: LLVMContextCreate), on
    /// failure too. Returns a handle for release(), or null with the error
    /// written to `err` (NUL-terminated, cut to `err_cap`).
    compile: *const fn (module: ?*anyopaque, opt_level: u32, err: [*]u8, err_cap: usize) callconv(.c) ?*anyopaque,
    /// The address of a function or global of a compiled module, by name
    /// (0 if there is none). Names are global to the process: a consumer
    /// keeps its own unique.
    lookup: *const fn (name: [*:0]const u8) callconv(.c) u64,
    /// Make native functions (a runtime's helpers) callable from compiled
    /// code by name. 0, or -1 with the error in `err`.
    define: *const fn (names: [*]const [*:0]const u8, addrs: [*]const u64, n: usize, err: [*]u8, err_cap: usize) callconv(.c) i32,
    /// Free a compiled module's code (its addresses become invalid).
    release: *const fn (handle: ?*anyopaque) callconv(.c) void,
    /// Compile a module to an object file for a target (null: this
    /// process's), CPU and features (null: the host's, or generic for another
    /// target). Takes the module and its context, as compile() does. Returns
    /// the bytes (free them with free_bytes) and their length, or null with
    /// the error in `err`.
    emit_object: *const fn (module: ?*anyopaque, opt_level: u32, triple: ?[*:0]const u8, cpu: ?[*:0]const u8, features: ?[*:0]const u8, out_len: *usize, err: [*]u8, err_cap: usize) callconv(.c) ?[*]u8,
    free_bytes: *const fn (bytes: ?[*]u8) callconv(.c) void,
    /// The JIT's target triple ("x86_64-unknown-linux-gnu") and data layout;
    /// null if the JIT can't start (then compile() says why).
    triple: *const fn () callconv(.c) ?[*:0]const u8,
    data_layout: *const fn () callconv(.c) ?[*:0]const u8,
    /// Add an object file emit_object() made for this process (compiled
    /// code kept from before: a cache) to the JIT, as compile() adds a
    /// module; the bytes are copied. Returns a handle for release(), or
    /// null with the error in `err`. (ABI 2)
    load_object: *const fn (bytes: [*]const u8, len: usize, err: [*]u8, err_cap: usize) callconv(.c) ?*anyopaque,
};

pub const view = LlvmView{
    .llvm_version = jc.LLVM_VERSION,
    .function = &function,
    .compile = &compile,
    .lookup = &lookup,
    .define = &define,
    .release = &release,
    .emit_object = &emitObject,
    .free_bytes = &freeBytes,
    .triple = &triple,
    .data_layout = &dataLayout,
    .load_object = &loadObject,
};

/// The C API functions the capsule exports: building modules (contexts,
/// types, constants, globals, functions, blocks, instructions, attributes,
/// intrinsics), checking and printing them, reading IR text (for tests and
/// debugging), copying a module into a context of its own (as bitcode: to
/// compile it on another thread).
pub const api_names = [_][]const u8{
    // Contexts and modules
    "LLVMContextCreate",                 "LLVMContextDispose",              "LLVMModuleCreateWithNameInContext",
    "LLVMDisposeModule",                 "LLVMCloneModule",                 "LLVMGetModuleContext",
    "LLVMPrintModuleToString",           "LLVMPrintValueToString",          "LLVMPrintTypeToString",
    "LLVMDisposeMessage",                "LLVMVerifyModule",                "LLVMVerifyFunction",
    "LLVMSetTarget",                     "LLVMSetDataLayout",               "LLVMGetNamedFunction",
    "LLVMGetNamedGlobal",                "LLVMCreateMemoryBufferWithMemoryRangeCopy", "LLVMParseIRInContext",
    // Bitcode (a module copied into another context)
    "LLVMWriteBitcodeToMemoryBuffer",    "LLVMParseBitcodeInContext2",      "LLVMDisposeMemoryBuffer",
    // The host (what object files emit_object makes for it are for: a
    // consumer keeping them notes it)
    "LLVMGetHostCPUName",                "LLVMGetHostCPUFeatures",
    // Types
    "LLVMInt1TypeInContext",             "LLVMInt8TypeInContext",           "LLVMInt16TypeInContext",
    "LLVMInt32TypeInContext",            "LLVMInt64TypeInContext",          "LLVMIntTypeInContext",
    "LLVMFloatTypeInContext",            "LLVMDoubleTypeInContext",         "LLVMVoidTypeInContext",
    "LLVMPointerTypeInContext",          "LLVMStructTypeInContext",         "LLVMStructCreateNamed",
    "LLVMStructSetBody",                 "LLVMArrayType2",                  "LLVMVectorType",
    "LLVMFunctionType",                  "LLVMTypeOf",                      "LLVMGlobalGetValueType",
    "LLVMGetTypeKind",                   "LLVMGetIntTypeWidth",             "LLVMGetReturnType",
    "LLVMCountParamTypes",
    // Constants
    "LLVMConstInt",                      "LLVMConstReal",                   "LLVMConstNull",
    "LLVMConstAllOnes",                  "LLVMGetUndef",                    "LLVMGetPoison",
    "LLVMConstPointerNull",              "LLVMConstStringInContext2",       "LLVMConstStructInContext",
    "LLVMConstNamedStruct",              "LLVMConstArray2",                 "LLVMConstIntToPtr",
    "LLVMConstPtrToInt",                 "LLVMConstBitCast",                "LLVMConstGEP2",
    "LLVMConstInBoundsGEP2",             "LLVMConstIntGetSExtValue",        "LLVMConstIntGetZExtValue",
    "LLVMIsConstant",                    "LLVMIsAConstantInt",
    // Globals and values
    "LLVMAddGlobal",                     "LLVMAddAlias2",                   "LLVMSetInitializer",              "LLVMSetGlobalConstant",
    "LLVMSetLinkage",                    "LLVMSetUnnamedAddress",           "LLVMSetAlignment",
    "LLVMSetVisibility",                 "LLVMSetValueName2",               "LLVMGetValueName2",
    "LLVMReplaceAllUsesWith",            "LLVMInstructionEraseFromParent",
    // Functions and attributes
    "LLVMAddFunction",                   "LLVMDeleteFunction",              "LLVMGetParam",
    "LLVMCountParams",                   "LLVMSetFunctionCallConv",         "LLVMGetEnumAttributeKindForName",
    "LLVMCreateEnumAttribute",           "LLVMAddAttributeAtIndex",         "LLVMAddCallSiteAttribute",
    "LLVMLookupIntrinsicID",             "LLVMGetIntrinsicDeclaration",     "LLVMGetEntryBasicBlock",
    // Basic blocks
    "LLVMAppendBasicBlockInContext",     "LLVMGetInsertBlock",              "LLVMGetBasicBlockTerminator",
    "LLVMGetFirstInstruction",           "LLVMGetLastInstruction",          "LLVMGetBasicBlockParent",
    "LLVMDeleteBasicBlock",              "LLVMMoveBasicBlockAfter",
    // The builder
    "LLVMCreateBuilderInContext",        "LLVMDisposeBuilder",              "LLVMPositionBuilderAtEnd",
    "LLVMPositionBuilderBefore",         "LLVMBuildRet",                    "LLVMBuildRetVoid",
    "LLVMBuildBr",                       "LLVMBuildCondBr",                 "LLVMBuildSwitch",
    "LLVMAddCase",                       "LLVMBuildUnreachable",            "LLVMBuildAdd",
    "LLVMBuildNSWAdd",                   "LLVMBuildSub",                    "LLVMBuildNSWSub",
    "LLVMBuildMul",                      "LLVMBuildNSWMul",                 "LLVMBuildSDiv",
    "LLVMBuildUDiv",                     "LLVMBuildSRem",                   "LLVMBuildURem",
    "LLVMBuildFAdd",                     "LLVMBuildFSub",                   "LLVMBuildFMul",
    "LLVMBuildFDiv",                     "LLVMBuildFRem",                   "LLVMBuildFNeg",
    "LLVMBuildNeg",                      "LLVMBuildNot",                    "LLVMBuildAnd",
    "LLVMBuildOr",                       "LLVMBuildXor",                    "LLVMBuildShl",
    "LLVMBuildLShr",                     "LLVMBuildAShr",                   "LLVMBuildICmp",
    "LLVMBuildFCmp",                     "LLVMBuildAlloca",                 "LLVMBuildArrayAlloca",
    "LLVMBuildLoad2",                    "LLVMBuildStore",                  "LLVMBuildGEP2",
    "LLVMBuildInBoundsGEP2",             "LLVMBuildStructGEP2",             "LLVMBuildTrunc",
    "LLVMBuildZExt",                     "LLVMBuildSExt",                   "LLVMBuildFPToSI",
    "LLVMBuildSIToFP",                   "LLVMBuildFPTrunc",                "LLVMBuildFPExt",
    "LLVMBuildBitCast",                  "LLVMBuildPtrToInt",               "LLVMBuildIntToPtr",
    "LLVMBuildPhi",                      "LLVMAddIncoming",                 "LLVMBuildCall2",
    "LLVMBuildSelect",                   "LLVMBuildExtractValue",           "LLVMBuildInsertValue",
    "LLVMBuildMemCpy",                   "LLVMBuildMemSet",
};

fn function(name: [*:0]const u8) callconv(.c) ?*const anyopaque {
    const want = std.mem.span(name);
    inline for (api_names) |n| {
        if (std.mem.eql(u8, n, want)) return @ptrCast(&@field(c, n));
    }
    return null;
}

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

/// Free a module handed over and its context.
fn dispose(module: c.LLVMModuleRef) void {
    const ctx = c.LLVMGetModuleContext(module);
    c.LLVMDisposeModule(module);
    c.LLVMContextDispose(ctx);
}

/// Whether a module handed over is valid (else the error, and it's freed).
fn verified(module: c.LLVMModuleRef, err: [*]u8, cap: usize) bool {
    var msg: [*c]u8 = null;
    defer if (msg) |m| c.LLVMDisposeMessage(m);
    if (c.LLVMVerifyModule(module, c.LLVMReturnStatusAction, &msg) != 0) {
        setError(err, cap, if (msg) |m| std.mem.span(m) else "the module isn't valid");
        dispose(module);
        return false;
    }
    return true;
}

fn pipeline(opt_level: u32) [*:0]const u8 {
    return switch (opt_level) {
        0 => "default<O0>",
        1 => "default<O1>",
        2 => "default<O2>",
        else => "default<O3>",
    };
}

fn compile(module_opt: ?*anyopaque, opt_level: u32, err: [*]u8, err_cap: usize) callconv(.c) ?*anyopaque {
    const module: c.LLVMModuleRef = @ptrCast(module_opt orelse {
        setError(err, err_cap, "no module");
        return null;
    });
    const j = jit(err, err_cap) orelse {
        dispose(module);
        return null;
    };
    if (!verified(module, err, err_cap)) return null;
    // (outside the lock: modules of their own contexts optimize in parallel)
    jc.optimizeModuleWith(j, module, pipeline(opt_level)) catch {
        dispose(module);
        setError(err, err_cap, "LLVM's optimizer failed");
        return null;
    };
    const ctx = c.LLVMGetModuleContext(module);
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

fn emitObject(module_opt: ?*anyopaque, opt_level: u32, triple_opt: ?[*:0]const u8, cpu_opt: ?[*:0]const u8, features_opt: ?[*:0]const u8, out_len: *usize, err: [*]u8, err_cap: usize) callconv(.c) ?[*]u8 {
    const module: c.LLVMModuleRef = @ptrCast(module_opt orelse {
        setError(err, err_cap, "no module");
        return null;
    });
    const j = jit(err, err_cap) orelse {
        dispose(module);
        return null;
    };
    if (!verified(module, err, err_cap)) return null;
    defer dispose(module);
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

fn loadObject(bytes: [*]const u8, len: usize, err: [*]u8, err_cap: usize) callconv(.c) ?*anyopaque {
    const j = jit(err, err_cap) orelse return null;
    // (the JIT takes the buffer)
    const buf = c.LLVMCreateMemoryBufferWithMemoryRangeCopy(bytes, len, "zgram.object");
    jc.lockJit();
    defer jc.unlockJit();
    const dylib = c.LLVMOrcLLJITGetMainJITDylib(j);
    const tracker = c.LLVMOrcJITDylibCreateResourceTracker(dylib);
    if (c.LLVMOrcLLJITAddObjectFileWithRT(j, tracker, buf)) |e| {
        setLlvmError(err, err_cap, e);
        c.LLVMOrcReleaseResourceTracker(tracker);
        return null;
    }
    return @ptrCast(tracker);
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
