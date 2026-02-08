//! Ergonomic Zig wrapper around the LLVM C API for building IR in memory.
//!
//! Designed specifically for zgram's JIT codegen needs. Provides a clean
//! Zig-native API that hides the C pointer gymnastics and null-terminated
//! string requirements.

const std = @import("std");
const Allocator = std.mem.Allocator;

// LLVM C API bindings
const c = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/LLJIT.h");
    @cInclude("llvm-c/Orc.h");
    @cInclude("llvm-c/Error.h");
    @cInclude("llvm-c/Target.h");
    @cInclude("llvm-c/TargetMachine.h");
    @cInclude("llvm-c/Transforms/PassBuilder.h");
});

// Re-export C bindings and key types for use by codegen/compiler
pub const llvm = c;
pub const LLVMModuleRef = c.LLVMModuleRef;
pub const LLVMContextRef = c.LLVMContextRef;

// Opaque handle types exposed to callers
pub const Value = c.LLVMValueRef;
pub const Type = c.LLVMTypeRef;
pub const Block = c.LLVMBasicBlockRef;

/// Integer comparison predicates.
pub const ICmp = enum(c_uint) {
    eq = c.LLVMIntEQ,
    ne = c.LLVMIntNE,
    ugt = c.LLVMIntUGT,
    uge = c.LLVMIntUGE,
    ult = c.LLVMIntULT,
    ule = c.LLVMIntULE,
    sgt = c.LLVMIntSGT,
    sge = c.LLVMIntSGE,
    slt = c.LLVMIntSLT,
    sle = c.LLVMIntSLE,
};

/// Builder wraps an LLVM context, module, and IR builder for constructing
/// LLVM IR in memory. Call `deinit()` only if you do NOT pass the module
/// to the JIT (the JIT takes ownership).
pub const Builder = struct {
    ctx: c.LLVMContextRef,
    module: c.LLVMModuleRef,
    b: c.LLVMBuilderRef,
    allocator: Allocator,

    // Cached common types
    i1: Type,
    i8: Type,
    i16: Type,
    i32: Type,
    i64: Type,
    ptr: Type,
    void: Type,

    // Current function being built (for appending blocks)
    current_fn: Value = null,

    // Block name counter for uniqueness
    block_counter: u32 = 0,

    /// Create a new builder with a fresh LLVM context and module.
    pub fn init(allocator: Allocator, module_name: [*:0]const u8) Builder {
        const ctx = c.LLVMContextCreate();
        const module = c.LLVMModuleCreateWithNameInContext(module_name, ctx);
        const b = c.LLVMCreateBuilderInContext(ctx);

        return .{
            .ctx = ctx,
            .module = module,
            .b = b,
            .allocator = allocator,
            .i1 = c.LLVMInt1TypeInContext(ctx),
            .i8 = c.LLVMInt8TypeInContext(ctx),
            .i16 = c.LLVMInt16TypeInContext(ctx),
            .i32 = c.LLVMInt32TypeInContext(ctx),
            .i64 = c.LLVMInt64TypeInContext(ctx),
            .ptr = c.LLVMPointerTypeInContext(ctx, 0),
            .void = c.LLVMVoidTypeInContext(ctx),
        };
    }

    /// Dispose of the builder. Only call if you did NOT pass the module to JIT.
    pub fn deinit(self: *Builder) void {
        c.LLVMDisposeBuilder(self.b);
        c.LLVMDisposeModule(self.module);
        c.LLVMContextDispose(self.ctx);
    }

    /// Dispose only the IR builder (after handing module+ctx to JIT).
    pub fn deinitBuilderOnly(self: *Builder) void {
        c.LLVMDisposeBuilder(self.b);
    }

    /// Dump the module IR to stderr (for debugging).
    pub fn dump(self: *const Builder) void {
        const ir = c.LLVMPrintModuleToString(self.module);
        defer c.LLVMDisposeMessage(ir);
        std.debug.print("{s}\n", .{ir});
    }

    // ── Types ──

    /// Create a function type.
    pub fn fnType(self: *const Builder, return_ty: Type, params: []const Type) Type {
        _ = self;
        return c.LLVMFunctionType(return_ty, @constCast(params.ptr), @intCast(params.len), 0);
    }

    /// Create an array type.
    pub fn arrayType(self: *const Builder, elem: Type, count: u64) Type {
        _ = self;
        return c.LLVMArrayType2(elem, count);
    }

    // ── Functions ──

    /// Add a function to the module.
    pub fn addFunction(self: *Builder, name: [*:0]const u8, ty: Type) Value {
        return c.LLVMAddFunction(self.module, name, ty);
    }

    /// Set the current function (for appending blocks).
    pub fn setCurrentFn(self: *Builder, func: Value) void {
        self.current_fn = func;
    }

    /// Get function parameter by index.
    pub fn param(self: *const Builder, func: Value, index: u32) Value {
        _ = self;
        return c.LLVMGetParam(func, index);
    }

    /// Set linkage on a function or global.
    pub fn setLinkageInternal(self: *const Builder, val: Value) void {
        _ = self;
        c.LLVMSetLinkage(val, c.LLVMInternalLinkage);
    }

    // ── Blocks ──

    /// Append a basic block to the current function.
    pub fn appendBlock(self: *Builder, name: [*:0]const u8) Block {
        return c.LLVMAppendBasicBlockInContext(self.ctx, self.current_fn, name);
    }

    /// Append a block with an auto-generated unique name.
    pub fn newBlock(self: *Builder, prefix: []const u8) !Block {
        const name = try std.fmt.allocPrintSentinel(self.allocator, "{s}_{d}", .{ prefix, self.block_counter }, 0);
        defer self.allocator.free(name);
        self.block_counter += 1;
        return c.LLVMAppendBasicBlockInContext(self.ctx, self.current_fn, name.ptr);
    }

    /// Position the builder at the end of a block.
    pub fn positionAtEnd(self: *Builder, block: Block) void {
        c.LLVMPositionBuilderAtEnd(self.b, block);
    }

    /// Get the current insert block.
    pub fn getCurrentBlock(self: *const Builder) Block {
        return c.LLVMGetInsertBlock(self.b);
    }

    // ── Constants ──

    /// Create an integer constant. For negative values, pass the bit pattern.
    pub fn constInt(self: *const Builder, ty: Type, val: u64) Value {
        _ = self;
        return c.LLVMConstInt(ty, val, 0);
    }

    /// Create a signed integer constant (sign-extended).
    pub fn constSInt(self: *const Builder, ty: Type, val: i64) Value {
        _ = self;
        return c.LLVMConstInt(ty, @bitCast(val), 1);
    }

    /// Create a null/zero constant.
    pub fn constNull(self: *const Builder, ty: Type) Value {
        _ = self;
        return c.LLVMConstNull(ty);
    }

    /// Create a constant array from values.
    pub fn constArray(self: *const Builder, elem_ty: Type, vals: []const Value) Value {
        _ = self;
        return c.LLVMConstArray2(elem_ty, @constCast(vals.ptr), @intCast(vals.len));
    }

    /// Create a constant string (no null terminator).
    pub fn constString(self: *const Builder, str: []const u8) Value {
        return c.LLVMConstStringInContext(self.ctx, str.ptr, @intCast(str.len), 1);
    }

    // ── Globals ──

    /// Add a global variable.
    pub fn addGlobal(self: *Builder, name: [*:0]const u8, ty: Type) Value {
        return c.LLVMAddGlobal(self.module, ty, name);
    }

    /// Add a global constant with initializer, internal linkage, unnamed addr.
    pub fn addGlobalConstant(self: *Builder, name: [*:0]const u8, ty: Type, init_val: Value) Value {
        const g = c.LLVMAddGlobal(self.module, ty, name);
        c.LLVMSetInitializer(g, init_val);
        c.LLVMSetGlobalConstant(g, 1);
        c.LLVMSetLinkage(g, c.LLVMInternalLinkage);
        c.LLVMSetUnnamedAddress(g, c.LLVMGlobalUnnamedAddr);
        return g;
    }

    /// Add a global string constant. Returns a pointer to the string data.
    pub fn addGlobalString(self: *Builder, name: [*:0]const u8, str: []const u8) Value {
        const str_val = self.constString(str);
        const str_ty = self.arrayType(self.i8, str.len);
        return self.addGlobalConstant(name, str_ty, str_val);
    }

    // ── Arithmetic ──

    pub fn add(self: *Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return c.LLVMBuildAdd(self.b, lhs, rhs, name);
    }

    pub fn sub(self: *Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return c.LLVMBuildSub(self.b, lhs, rhs, name);
    }

    pub fn shl(self: *Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return c.LLVMBuildShl(self.b, lhs, rhs, name);
    }

    pub fn lshr(self: *Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return c.LLVMBuildLShr(self.b, lhs, rhs, name);
    }

    pub fn @"and"(self: *Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return c.LLVMBuildAnd(self.b, lhs, rhs, name);
    }

    pub fn @"or"(self: *Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return c.LLVMBuildOr(self.b, lhs, rhs, name);
    }

    // ── Comparison ──

    pub fn icmp(self: *Builder, pred: ICmp, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return c.LLVMBuildICmp(self.b, @intFromEnum(pred), lhs, rhs, name);
    }

    // ── Memory ──

    pub fn load(self: *Builder, ty: Type, ptr_val: Value, alignment: u32, name: [*:0]const u8) Value {
        const inst = c.LLVMBuildLoad2(self.b, ty, ptr_val, name);
        if (alignment > 0) c.LLVMSetAlignment(inst, alignment);
        return inst;
    }

    pub fn store(self: *Builder, val: Value, ptr_val: Value, alignment: u32) Value {
        const inst = c.LLVMBuildStore(self.b, val, ptr_val);
        if (alignment > 0) c.LLVMSetAlignment(inst, alignment);
        return inst;
    }

    pub fn gep(self: *Builder, ty: Type, ptr_val: Value, indices: []const Value, name: [*:0]const u8) Value {
        return c.LLVMBuildInBoundsGEP2(self.b, ty, ptr_val, @constCast(indices.ptr), @intCast(indices.len), name);
    }

    // ── Vector Types & Constants ──

    /// Create a fixed-width vector type, e.g. <16 x i8>.
    pub fn vectorType(self: *const Builder, elem: Type, count: u32) Type {
        _ = self;
        return c.LLVMVectorType(elem, count);
    }

    /// Create a vector constant from an array of scalar values.
    pub fn constVector(self: *const Builder, vals: []const Value) Value {
        _ = self;
        return c.LLVMConstVector(@constCast(vals.ptr), @intCast(vals.len));
    }

    /// Create a splat vector: all lanes set to the same scalar value.
    pub fn splatVector(_: *const Builder, scalar: Value, width: u32) Value {
        var vals: [16]Value = undefined;
        for (0..width) |i| vals[i] = scalar;
        return c.LLVMConstVector(&vals, width);
    }

    // ── Casts ──

    pub fn trunc(self: *Builder, val: Value, dest_ty: Type, name: [*:0]const u8) Value {
        return c.LLVMBuildTrunc(self.b, val, dest_ty, name);
    }

    pub fn zext(self: *Builder, val: Value, dest_ty: Type, name: [*:0]const u8) Value {
        return c.LLVMBuildZExt(self.b, val, dest_ty, name);
    }

    pub fn bitcast(self: *Builder, val: Value, dest_ty: Type, name: [*:0]const u8) Value {
        return c.LLVMBuildBitCast(self.b, val, dest_ty, name);
    }

    // ── Control Flow ──

    pub fn ret(self: *Builder, val: Value) Value {
        return c.LLVMBuildRet(self.b, val);
    }

    pub fn retVoid(self: *Builder) Value {
        return c.LLVMBuildRetVoid(self.b);
    }

    pub fn br(self: *Builder, dest: Block) Value {
        return c.LLVMBuildBr(self.b, dest);
    }

    pub fn condBr(self: *Builder, cond: Value, then_bb: Block, else_bb: Block) Value {
        return c.LLVMBuildCondBr(self.b, cond, then_bb, else_bb);
    }

    // ── PHI Nodes ──

    pub fn phi(self: *Builder, ty: Type, name: [*:0]const u8) Value {
        return c.LLVMBuildPhi(self.b, ty, name);
    }

    pub fn addIncoming(self: *const Builder, phi_node: Value, vals: []const Value, blocks: []const Block) void {
        _ = self;
        std.debug.assert(vals.len == blocks.len);
        c.LLVMAddIncoming(phi_node, @constCast(vals.ptr), @constCast(blocks.ptr), @intCast(vals.len));
    }

    // ── Function Calls ──

    pub fn call(self: *Builder, fn_ty: Type, func: Value, args: []const Value, name: [*:0]const u8) Value {
        return c.LLVMBuildCall2(self.b, fn_ty, func, @constCast(args.ptr), @intCast(args.len), name);
    }

    // ── Intrinsics ──

    /// Look up an LLVM intrinsic by name. Returns 0 if not found.
    pub fn lookupIntrinsic(self: *const Builder, name: []const u8) u32 {
        _ = self;
        return c.LLVMLookupIntrinsicID(name.ptr, name.len);
    }

    /// Get a declaration of an intrinsic for specific parameter types.
    pub fn getIntrinsicDecl(self: *const Builder, id: u32, param_types: []const Type) Value {
        return c.LLVMGetIntrinsicDeclaration(self.module, id, @constCast(param_types.ptr), param_types.len);
    }

    /// Build a call to an intrinsic.
    pub fn callIntrinsic(self: *Builder, id: u32, param_types: []const Type, args: []const Value, name: [*:0]const u8) Value {
        const decl = self.getIntrinsicDecl(id, param_types);
        const fn_ty = c.LLVMIntrinsicGetType(self.ctx, id, @constCast(param_types.ptr), param_types.len);
        return self.call(fn_ty, decl, args, name);
    }

    // ── Xor ──

    pub fn xor(self: *Builder, lhs: Value, rhs: Value, name: [*:0]const u8) Value {
        return c.LLVMBuildXor(self.b, lhs, rhs, name);
    }

    // ── Module Dump ──

    /// Return the LLVM IR text for the entire module as a Zig slice.
    /// Caller must free the returned string with `allocator.free()`.
    pub fn printModule(self: *const Builder, allocator: Allocator) ![]const u8 {
        const raw: [*:0]u8 = c.LLVMPrintModuleToString(self.module) orelse return error.OutOfMemory;
        defer c.LLVMDisposeMessage(raw);
        const len = std.mem.len(raw);
        const buf = try allocator.alloc(u8, len);
        @memcpy(buf, raw[0..len]);
        return buf;
    }
};
