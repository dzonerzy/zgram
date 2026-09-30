// lexy expression benchmark, written the idiomatic lexy way: an
// expression_production with operator tables (like lexy's calculator
// example). Accepts the same language as zgram's expression grammar; lexy
// tells calls from names with lookahead instead of backtracking.
//
//   lexy_expr_bench file.txt          validation (lexy::match)
//   lexy_expr_bench file.txt --tree   lexy::parse_as_tree (its own tree shape)
#include <cstring>

#include <lexy/action/match.hpp>
#include <lexy/action/validate.hpp>
#include <lexy_ext/report_error.hpp>
#include <lexy/action/parse_as_tree.hpp>
#include <lexy/callback.hpp>
#include <lexy/dsl.hpp>
#include <lexy/input/string_input.hpp>
#include <lexy/parse_tree.hpp>

#include "harness.h"

namespace grammar {
namespace dsl = lexy::dsl;

struct expr;

struct number : lexy::token_production {
    static constexpr auto rule = [] {
        auto fraction = dsl::lit_c<'.'> >> dsl::digits<>;
        auto exponent = (dsl::lit_c<'e'> | dsl::lit_c<'E'>) >> dsl::sign + dsl::digits<>;
        return dsl::digits<> + dsl::opt(fraction) + dsl::opt(exponent);
    }();
};

struct name {
    static constexpr auto rule = dsl::identifier(dsl::ascii::alpha_underscore, dsl::ascii::alpha_digit_underscore);
};

struct call_args {
    static constexpr auto rule = dsl::parenthesized.opt_list(dsl::recurse<expr>, dsl::sep(dsl::comma));
};

struct paren {
    static constexpr auto rule = dsl::parenthesized(dsl::recurse<expr>);
};

struct expr : lexy::expression_production {
    // The benchmark inputs chain thousands of operators in one expression
    static constexpr auto max_operator_nesting = 1 << 20;

    static constexpr auto atom = [] {
        auto var_or_call = dsl::p<name> >> dsl::if_(dsl::p<call_args>);
        return dsl::p<paren> | var_or_call | dsl::peek(dsl::digit<>) >> dsl::p<number>;
    }();

    struct neg : dsl::prefix_op {
        static constexpr auto op = dsl::op(dsl::lit_c<'-'>);
        using operand = dsl::atom;
    };
    struct power : dsl::infix_op_right {
        static constexpr auto op = dsl::op(dsl::lit_c<'^'>);
        using operand = neg;
    };
    struct product : dsl::infix_op_left {
        static constexpr auto op = dsl::op(dsl::lit_c<'*'>) / dsl::op(dsl::lit_c<'/'>) / dsl::op(dsl::lit_c<'%'>);
        using operand = power;
    };
    struct sum : dsl::infix_op_left {
        static constexpr auto op = dsl::op(dsl::lit_c<'+'>) / dsl::op(dsl::lit_c<'-'>);
        using operand = product;
    };
    using operation = sum;
};

struct text {
    static constexpr auto max_recursion_depth = 4096;
    static constexpr auto whitespace = dsl::ascii::space;
    static constexpr auto rule = dsl::p<expr> + dsl::eof;
};
}  // namespace grammar

using input_t = lexy::string_input<lexy::utf8_encoding>;

struct ctx_t {
    input_t input;
    lexy::parse_tree_for<input_t> tree;
};

static int validate(void *p) {
    auto *ctx = static_cast<ctx_t *>(p);
    return lexy::match<grammar::text>(ctx->input) ? 1 : 0;
}

static int build_tree(void *p) {
    auto *ctx = static_cast<ctx_t *>(p);
    return lexy::parse_as_tree<grammar::text>(ctx->tree, ctx->input, lexy::noop).is_success() ? 1 : 0;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: lexy_expr_bench <expr_file> [--tree]\n");
        return 1;
    }
    char *data;
    size_t len = bench_read_file(argv[1], &data);
    ctx_t ctx{input_t(data, len), {}};
    if (!validate(&ctx)) {
        // Explain the rejection
        lexy::validate<grammar::text>(ctx.input, lexy_ext::report_error);
        return 1;
    }
    if (argc > 2 && std::strcmp(argv[2], "--tree") == 0) {
        build_tree(&ctx);
        size_t nodes = 0;
        for (auto [event, node] : ctx.tree.traverse()) {
            if (event != lexy::traverse_event::exit) ++nodes;
        }
        fprintf(stderr, "Tree nodes (productions and tokens): %zu\n", nodes);
        bench_run("lexy tree", len, build_tree, &ctx);
    } else {
        bench_run("lexy", len, validate, &ctx);
    }
    return 0;
}
