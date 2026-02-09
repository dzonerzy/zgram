//! Re-export module for standalone tools (benchmarks, CLI) that need
//! the zgram compiler pipeline without the Python/PyOZ bindings.

pub const grammar_parser = @import("grammar_parser.zig");
pub const jit_codegen = @import("jit_codegen.zig");
pub const jit_compiler = @import("jit_compiler.zig");
pub const jit_helpers = @import("jit_helpers.zig");
pub const parse_abi = @import("parse_abi.zig");

// Force the linker to keep JIT helper symbols (they're resolved at runtime
// by LLJIT via dlsym, so the linker would otherwise dead-strip them).
comptime {
    _ = jit_helpers.zgram_ensure_capacity;
    _ = jit_helpers.zgram_reserve_node;
    _ = jit_helpers.zgram_fill_node;
    _ = jit_helpers.zgram_set_error;
    _ = jit_helpers.zgram_set_error_at_hwm;
    _ = jit_helpers.zgram_set_rule_name;
}
