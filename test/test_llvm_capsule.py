"""zgram.llvm_capsule(): zgram's LLVM for other packages (zgram.llvm.v1):
LLVM's C API to build modules in memory, zgram's JIT to compile them."""

import ctypes

import pytest
import zgram

ERR = 512
P = ctypes.c_void_p


class LlvmView(ctypes.Structure):
    _fields_ = [
        ("abi", ctypes.c_uint32),
        ("llvm_version", ctypes.c_char_p),
        ("function", ctypes.CFUNCTYPE(P, ctypes.c_char_p)),
        ("compile", ctypes.CFUNCTYPE(P, P, ctypes.c_uint32, ctypes.c_char_p, ctypes.c_size_t)),
        ("lookup", ctypes.CFUNCTYPE(ctypes.c_uint64, ctypes.c_char_p)),
        ("define", ctypes.CFUNCTYPE(ctypes.c_int32, ctypes.POINTER(ctypes.c_char_p), ctypes.POINTER(ctypes.c_uint64), ctypes.c_size_t, ctypes.c_char_p, ctypes.c_size_t)),
        ("release", ctypes.CFUNCTYPE(None, P)),
        ("emit_object", ctypes.CFUNCTYPE(P, P, ctypes.c_uint32, ctypes.c_char_p, ctypes.c_char_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_size_t), ctypes.c_char_p, ctypes.c_size_t)),
        ("free_bytes", ctypes.CFUNCTYPE(None, P)),
        ("triple", ctypes.CFUNCTYPE(ctypes.c_char_p)),
        ("data_layout", ctypes.CFUNCTYPE(ctypes.c_char_p)),
        ("load_object", ctypes.CFUNCTYPE(P, ctypes.c_char_p, ctypes.c_size_t, ctypes.c_char_p, ctypes.c_size_t)),
    ]


class Api:
    """LLVM's C API functions, through the capsule."""

    SIGS = {
        "LLVMContextCreate": (P, []),
        "LLVMModuleCreateWithNameInContext": (P, [ctypes.c_char_p, P]),
        "LLVMInt64TypeInContext": (P, [P]),
        "LLVMFunctionType": (P, [P, ctypes.POINTER(P), ctypes.c_uint, ctypes.c_int]),
        "LLVMAddFunction": (P, [P, ctypes.c_char_p, P]),
        "LLVMAppendBasicBlockInContext": (P, [P, P, ctypes.c_char_p]),
        "LLVMCreateBuilderInContext": (P, [P]),
        "LLVMPositionBuilderAtEnd": (None, [P, P]),
        "LLVMGetParam": (P, [P, ctypes.c_uint]),
        "LLVMBuildAdd": (P, [P, P, P, ctypes.c_char_p]),
        "LLVMBuildRet": (P, [P, P]),
        "LLVMDisposeBuilder": (None, [P]),
        "LLVMCreateMemoryBufferWithMemoryRangeCopy": (P, [ctypes.c_char_p, ctypes.c_size_t, ctypes.c_char_p]),
        "LLVMParseIRInContext": (ctypes.c_int, [P, P, ctypes.POINTER(P), ctypes.POINTER(ctypes.c_char_p)]),
    }

    def __init__(self, view):
        for name, (res, args) in self.SIGS.items():
            addr = view.function(name.encode())
            assert addr, name
            setattr(self, name[4:], ctypes.CFUNCTYPE(res, *args)(addr))

    def parse(self, ir):
        """A module of its own context from IR text."""
        ctx = self.ContextCreate()
        buf = self.CreateMemoryBufferWithMemoryRangeCopy(ir.encode(), len(ir.encode()), b"test")
        module = P()
        msg = ctypes.c_char_p()
        assert self.ParseIRInContext(ctx, buf, ctypes.byref(module), ctypes.byref(msg)) == 0, msg.value
        return module


@pytest.fixture(scope="module")
def llvm():
    capsule = zgram.llvm_capsule()
    get = ctypes.pythonapi.PyCapsule_GetPointer
    get.restype = P
    get.argtypes = [ctypes.py_object, ctypes.c_char_p]
    view = ctypes.cast(get(capsule, b"zgram.llvm.v1"), ctypes.POINTER(LlvmView)).contents
    assert view.abi == zgram.LLVM_ABI == 2
    view._capsule = capsule  # (keep it alive)
    view.api = Api(view)
    return view


def compile_module(llvm, module, opt=2):
    err = ctypes.create_string_buffer(ERR)
    handle = llvm.compile(module, opt, err, ERR)
    return handle, err.value.decode()


def test_build_compile_and_call(llvm):
    # i64 add(i64 a, i64 b), built in memory through the C API
    api = llvm.api
    ctx = api.ContextCreate()
    module = api.ModuleCreateWithNameInContext(b"m", ctx)
    i64 = api.Int64TypeInContext(ctx)
    params = (P * 2)(i64, i64)
    fn = api.AddFunction(module, b"zgram_capsule_test_add", api.FunctionType(i64, params, 2, 0))
    b = api.CreateBuilderInContext(ctx)
    api.PositionBuilderAtEnd(b, api.AppendBasicBlockInContext(ctx, fn, b"entry"))
    api.BuildRet(b, api.BuildAdd(b, api.GetParam(fn, 0), api.GetParam(fn, 1), b"r"))
    api.DisposeBuilder(b)
    handle, err = compile_module(llvm, module)
    assert handle and err == ""
    add = ctypes.CFUNCTYPE(ctypes.c_int64, ctypes.c_int64, ctypes.c_int64)(llvm.lookup(b"zgram_capsule_test_add"))
    assert add(40, 2) == 42
    llvm.release(handle)
    assert llvm.lookup(b"zgram_capsule_test_add") == 0


def test_unknown_function(llvm):
    assert llvm.function(b"LLVMNoSuchThing") is None


def test_calling_a_native_function(llvm):
    # a runtime's helper, defined by name, called from the compiled code
    seen = []
    helper = ctypes.CFUNCTYPE(ctypes.c_int64, ctypes.c_int64)(lambda x: seen.append(x) or x * 10)
    names = (ctypes.c_char_p * 1)(b"zgram_capsule_test_helper")
    addrs = (ctypes.c_uint64 * 1)(ctypes.cast(helper, P).value)
    err = ctypes.create_string_buffer(ERR)
    assert llvm.define(names, addrs, 1, err, ERR) == 0, err.value
    module = llvm.api.parse("""
declare i64 @zgram_capsule_test_helper(i64)
define i64 @zgram_capsule_test_twice(i64 %x) {
  %a = call i64 @zgram_capsule_test_helper(i64 %x)
  %b = call i64 @zgram_capsule_test_helper(i64 %a)
  ret i64 %b
}
""")
    handle, err = compile_module(llvm, module, opt=0)
    assert handle, err
    f = ctypes.CFUNCTYPE(ctypes.c_int64, ctypes.c_int64)(llvm.lookup(b"zgram_capsule_test_twice"))
    assert f(3) == 300 and seen == [3, 30]
    llvm.release(handle)


def test_an_invalid_module(llvm):
    # a block without a terminator: refused (and the module freed)
    api = llvm.api
    ctx = api.ContextCreate()
    module = api.ModuleCreateWithNameInContext(b"m", ctx)
    i64 = api.Int64TypeInContext(ctx)
    fn = api.AddFunction(module, b"zgram_capsule_bad", api.FunctionType(i64, None, 0, 0))
    api.AppendBasicBlockInContext(ctx, fn, b"entry")
    handle, err = compile_module(llvm, module)
    assert not handle and "terminator" in err


def test_target_and_object(llvm):
    triple = llvm.triple().decode()
    assert triple.startswith("x86_64")
    assert llvm.data_layout()
    ir = "define i32 @answer() {\n  ret i32 42\n}\n"
    n = ctypes.c_size_t(0)
    err = ctypes.create_string_buffer(ERR)
    ptr = llvm.emit_object(llvm.api.parse(ir), 2, None, None, None, ctypes.byref(n), err, ERR)
    assert ptr and n.value > 0, err.value
    data = ctypes.string_at(ptr, n.value)
    llvm.free_bytes(ptr)
    # this platform's object format
    assert data[:4] == b"\x7fELF" or data[:2] == b"\x64\x86"
    # and another target's
    ptr = llvm.emit_object(llvm.api.parse(ir), 2, b"x86_64-pc-windows-msvc", None, None, ctypes.byref(n), err, ERR)
    assert ptr, err.value
    assert ctypes.string_at(ptr, 2) == b"\x64\x86"  # COFF, x86-64
    llvm.free_bytes(ptr)


def test_object_loaded(llvm):
    # an object file made before (a cache of compiled code), into the JIT,
    # calling a native function defined by name
    names = (ctypes.c_char_p * 1)(b"zgram_capsule_obj_base")
    base = ctypes.c_int64(35)
    addrs = (ctypes.c_uint64 * 1)(ctypes.addressof(base))
    err = ctypes.create_string_buffer(ERR)
    assert llvm.define(names, addrs, 1, err, ERR) == 0, err.value
    ir = """
@zgram_capsule_obj_base = external global i64
define i64 @zgram_capsule_obj_answer(i64 %x) {
  %b = load i64, ptr @zgram_capsule_obj_base
  %r = add i64 %b, %x
  ret i64 %r
}
"""
    n = ctypes.c_size_t(0)
    ptr = llvm.emit_object(llvm.api.parse(ir), 2, None, None, None, ctypes.byref(n), err, ERR)
    assert ptr, err.value
    data = ctypes.string_at(ptr, n.value)
    llvm.free_bytes(ptr)
    handle = llvm.load_object(data, len(data), err, ERR)
    assert handle, err.value
    f = ctypes.CFUNCTYPE(ctypes.c_int64, ctypes.c_int64)(llvm.lookup(b"zgram_capsule_obj_answer"))
    assert f(7) == 42
    llvm.release(handle)
    assert llvm.lookup(b"zgram_capsule_obj_answer") == 0
    # bytes that aren't an object: an error
    assert not llvm.load_object(b"nonsense", 8, err, ERR) and err.value
