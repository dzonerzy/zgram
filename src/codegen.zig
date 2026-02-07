//! Code generator: converts grammar IR into a Zig source file (grammar_data.zig).
//!
//! The generated file defines comptime constants that the parser engine consumes.

const std = @import("std");
const Allocator = std.mem.Allocator;
const gp = @import("grammar_parser.zig");

/// Generate grammar_data.zig source from a Grammar IR.
/// Caller owns the returned slice and must free it with `allocator`.
pub fn generateZigSource(allocator: Allocator, grammar: *const gp.Grammar) ![]const u8 {
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);

    const w = buf.writer(allocator);

    try w.writeAll("//! Auto-generated grammar data. Do not edit.\n");
    try w.writeAll("const types = @import(\"grammar_types.zig\");\n");
    try w.writeAll("const Expr = types.Expr;\n");
    try w.writeAll("const Rule = types.Rule;\n");
    try w.writeAll("const CharRange = types.CharRange;\n");
    try w.writeAll("\n");

    // Build rule name -> index map
    // (We just search linearly since grammars are small)

    // Generate expressions and rules
    for (grammar.rules) |rule| {
        try w.writeAll("const rule_");
        try writeSafeName(w, rule.name);
        try w.writeAll("_expr: Expr = ");
        try genExpr(w, rule.expr, grammar.rules);
        try w.writeAll(";\n\n");
    }

    // Generate the grammar constant
    try w.writeAll("pub const grammar: types.Grammar = .{\n");
    try w.writeAll("    .rules = &.{\n");
    for (grammar.rules) |rule| {
        try w.writeAll("        .{ .name = \"");
        try w.writeAll(rule.name);
        try w.writeAll("\", .expr = rule_");
        try writeSafeName(w, rule.name);
        try w.writeAll("_expr },\n");
    }
    try w.writeAll("    },\n");
    try w.writeAll("};\n");

    return try allocator.dupe(u8, buf.items);
}

fn writeSafeName(writer: anytype, name: []const u8) !void {
    for (name) |c| {
        if (c == '-' or c == '.') {
            try writer.writeByte('_');
        } else {
            try writer.writeByte(c);
        }
    }
}

fn genExpr(writer: anytype, expr: *const gp.Expr, rules: []const *gp.Rule) !void {
    switch (expr.tag) {
        .literal => {
            try writer.writeAll(".{ .tag = .literal, .literal_value = \"");
            if (expr.literal_value) |v| {
                try zigStringEscape(writer, v);
            }
            try writer.writeAll("\" }");
        },
        .char_class => {
            try writer.writeAll(".{ .tag = .char_class, .char_ranges = &.{ ");
            if (expr.char_ranges) |ranges| {
                for (ranges, 0..) |r, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try writer.print(".{{ .start = {d}, .end = {d} }}", .{ r.start, r.end });
                }
            }
            try writer.writeAll(" }, .char_negated = ");
            if (expr.char_negated) {
                try writer.writeAll("true");
            } else {
                try writer.writeAll("false");
            }
            try writer.writeAll(" }");
        },
        .reference => {
            // Resolve reference name to index
            const name = expr.ref_name orelse return error.InvalidExpr;
            var idx: usize = 0;
            var found = false;
            for (rules, 0..) |rule, i| {
                if (std.mem.eql(u8, rule.name, name)) {
                    idx = i;
                    found = true;
                    break;
                }
            }
            if (!found) return error.UndefinedRule;
            try writer.print(".{{ .tag = .reference, .ref_index = {d} }}", .{idx});
        },
        .sequence => {
            try writer.writeAll(".{ .tag = .sequence, .children = &.{ ");
            if (expr.children) |children| {
                for (children, 0..) |child, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try genExpr(writer, child, rules);
                }
            }
            try writer.writeAll(" } }");
        },
        .alternative => {
            try writer.writeAll(".{ .tag = .alternative, .children = &.{ ");
            if (expr.children) |children| {
                for (children, 0..) |child, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try genExpr(writer, child, rules);
                }
            }
            try writer.writeAll(" } }");
        },
        .repetition => {
            try writer.writeAll(".{ .tag = .repetition, .rep_expr = &(");
            if (expr.rep_expr) |sub| {
                try genExpr(writer, sub, rules);
            }
            try writer.print("), .rep_kind = {d} }}", .{expr.rep_kind});
        },
        .not_predicate => {
            try writer.writeAll(".{ .tag = .not_predicate, .pred_expr = &(");
            if (expr.pred_expr) |sub| {
                try genExpr(writer, sub, rules);
            }
            try writer.writeAll(") }");
        },
        .and_predicate => {
            try writer.writeAll(".{ .tag = .and_predicate, .pred_expr = &(");
            if (expr.pred_expr) |sub| {
                try genExpr(writer, sub, rules);
            }
            try writer.writeAll(") }");
        },
        .any_char => {
            try writer.writeAll(".{ .tag = .any_char }");
        },
    }
}

fn zigStringEscape(writer: anytype, s: []const u8) !void {
    for (s) |ch| {
        switch (ch) {
            '\\' => try writer.writeAll("\\\\"),
            '"' => try writer.writeAll("\\\""),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            else => {
                if (ch < 0x20 or ch > 0x7e) {
                    try writer.print("\\x{x:0>2}", .{ch});
                } else {
                    try writer.writeByte(ch);
                }
            },
        }
    }
}
