//! Entry point template for compiled grammar .so files.
//! Codegen produces grammar_data.zig with the grammar constants.
//! This file wires the engine to the data and exports the C ABI function.

const abi = @import("parse_abi.zig");
const engine = @import("parser_engine.zig");
const data = @import("grammar_data.zig");

const GrammarParser = engine.Parser(data.grammar);

export fn zgram_parse(
    input_ptr: [*]const u8,
    input_len: usize,
    output: *abi.ParseOutput,
) callconv(.c) i32 {
    const input = input_ptr[0..input_len];
    return GrammarParser.parse(input, output);
}
