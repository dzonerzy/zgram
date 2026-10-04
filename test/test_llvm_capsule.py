"""zgram.llvm_capsule(): zgram's LLVM for other packages (zgram.llvm.v1)."""

import ctypes

import pytest
import zgram

ERR = 512


class LlvmView(ctypes.Structure):
    _fields_ = [
        ("abi", ctypes.c_uint32),
        ("llvm_version", ctypes.c_char_p),
        ("compile", ctypes.CFUNCTYPE(ctypes.c_void_p, ctypes.c_char_p, ctypes.c_size_t, ctypes.c_uint32, ctypes.c_char_p, ctypes.c_size_t)),
        ("lookup", ctypes.CFUNCTYPE(ctypes.c_uint64, ctypes.c_char_p)),
        ("define", ctypes.CFUNCTYPE(ctypes.c_int32, ctypes.POINTER(ctypes.c_char_p), ctypes.POINTER(ctypes.c_uint64), ctypes.c_size_t, ctypes.c_char_p, ctypes.c_size_t)),
        ("release", ctypes.CFUNCTYPE(None, ctypes.c_void_p)),
        ("emit_object", ctypes.CFUNCTYPE(ctypes.c_void_p, ctypes.c_char_p, ctypes.c_size_t, ctypes.c_uint32, ctypes.c_char_p, ctypes.c_char_p, ctypes.c_char_p, ctypes.POINTER(ctypes.c_size_t), ctypes.c_char_p, ctypes.c_size_t)),
        ("free_bytes", ctypes.CFUNCTYPE(None, ctypes.c_void_p)),
        ("triple", ctypes.CFUNCTYPE(ctypes.c_char_p)),
        ("data_layout", ctypes.CFUNCTYPE(ctypes.c_char_p)),
    ]


@pytest.fixture(scope="module")
def llvm():
    capsule = zgram.llvm_capsule()
    get = ctypes.pythonapi.PyCapsule_GetPointer
    get.restype = ctypes.c_void_p
    get.argtypes = [ctypes.py_object, ctypes.c_char_p]
    view = ctypes.cast(get(capsule, b"zgram.llvm.v1"), ctypes.POINTER(LlvmView)).contents
    assert view.abi == zgram.LLVM_ABI == 1
    view._capsule = capsule  # (keep it alive)
    return view


def compile_ir(llvm, ir, opt=2):
    err = ctypes.create_string_buffer(ERR)
    handle = llvm.compile(ir.encode(), len(ir.encode()), opt, err, ERR)
    return handle, err.value.decode()


def test_compile_and_call(llvm):
    ir = """
define i64 @zgram_capsule_test_add(i64 %a, i64 %b) {
  %r = add i64 %a, %b
  ret i64 %r
}
"""
    handle, err = compile_ir(llvm, ir)
    assert handle and err == ""
    addr = llvm.lookup(b"zgram_capsule_test_add")
    add = ctypes.CFUNCTYPE(ctypes.c_int64, ctypes.c_int64, ctypes.c_int64)(addr)
    assert add(40, 2) == 42
    llvm.release(handle)
    assert llvm.lookup(b"zgram_capsule_test_add") == 0


def test_calling_a_native_function(llvm):
    # a runtime's helper, defined by name, called from the IR
    seen = []
    helper_type = ctypes.CFUNCTYPE(ctypes.c_int64, ctypes.c_int64)
    helper = helper_type(lambda x: seen.append(x) or x * 10)
    names = (ctypes.c_char_p * 1)(b"zgram_capsule_test_helper")
    addrs = (ctypes.c_uint64 * 1)(ctypes.cast(helper, ctypes.c_void_p).value)
    err = ctypes.create_string_buffer(ERR)
    assert llvm.define(names, addrs, 1, err, ERR) == 0, err.value
    ir = """
declare i64 @zgram_capsule_test_helper(i64)
define i64 @zgram_capsule_test_twice(i64 %x) {
  %a = call i64 @zgram_capsule_test_helper(i64 %x)
  %b = call i64 @zgram_capsule_test_helper(i64 %a)
  ret i64 %b
}
"""
    handle, err = compile_ir(llvm, ir, opt=0)
    assert handle, err
    f = ctypes.CFUNCTYPE(ctypes.c_int64, ctypes.c_int64)(llvm.lookup(b"zgram_capsule_test_twice"))
    assert f(3) == 300 and seen == [3, 30]
    llvm.release(handle)


def test_errors(llvm):
    handle, err = compile_ir(llvm, "define i64 @f( {")
    assert not handle and "expected" in err
    # parses, but isn't valid: a block without a terminator
    handle, err = compile_ir(llvm, "define void @zgram_capsule_bad() {\nentry:\n  %x = add i64 1, 2\n}\n")
    assert not handle and err


def test_target_and_object(llvm):
    triple = llvm.triple().decode()
    assert triple.startswith("x86_64")
    assert llvm.data_layout()
    ir = "define i32 @answer() {\n  ret i32 42\n}\n"
    n = ctypes.c_size_t(0)
    err = ctypes.create_string_buffer(ERR)
    ptr = llvm.emit_object(ir.encode(), len(ir), 2, None, None, None, ctypes.byref(n), err, ERR)
    assert ptr and n.value > 0, err.value
    data = ctypes.string_at(ptr, n.value)
    llvm.free_bytes(ptr)
    # this platform's object format
    assert data[:4] in (b"\x7fELF", b"\x64\x86\x00\x00"[:4]) or data[:2] == b"\x64\x86"
    # and another target's
    ptr = llvm.emit_object(ir.encode(), len(ir), 2, b"x86_64-pc-windows-msvc", None, None, ctypes.byref(n), err, ERR)
    assert ptr, err.value
    assert ctypes.string_at(ptr, 2) == b"\x64\x86"  # COFF, x86-64
    llvm.free_bytes(ptr)
