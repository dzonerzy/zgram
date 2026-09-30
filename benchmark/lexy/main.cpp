// lexy JSON benchmark: validation (lexy::match) and parse tree
// (lexy::parse_as_tree). Grammar adapted from lexy's examples/json.cpp,
// without the productions' value callbacks.
#include <cstring>
#include <string>

#include <lexy/action/match.hpp>
#include <lexy/action/parse_as_tree.hpp>
#include <lexy/callback.hpp>
#include <lexy/dsl.hpp>
#include <lexy/input/string_input.hpp>
#include <lexy/parse_tree.hpp>

#include "harness.h"

namespace grammar {
namespace dsl = lexy::dsl;

struct json_value;

struct null_ : lexy::token_production {
    static constexpr auto rule = LEXY_LIT("null");
};
struct true_ : lexy::token_production {
    static constexpr auto rule = LEXY_LIT("true");
};
struct false_ : lexy::token_production {
    static constexpr auto rule = LEXY_LIT("false");
};

struct number : lexy::token_production {
    static constexpr auto rule = [] {
        auto integer = dsl::minus_sign + dsl::digits<>.no_leading_zero();
        auto fraction = dsl::lit_c<'.'> >> dsl::digits<>;
        auto exp_char = dsl::lit_c<'e'> | dsl::lit_c<'E'>;
        auto exponent = exp_char >> dsl::sign + dsl::digits<>;
        return dsl::peek(dsl::lit_c<'-'> / dsl::digit<>) >> integer + dsl::opt(fraction) + dsl::opt(exponent);
    }();
};

struct string : lexy::token_production {
    static constexpr auto escaped_symbols = lexy::symbol_table<char>
                                                .map<'"'>('"')
                                                .map<'\\'>('\\')
                                                .map<'/'>('/')
                                                .map<'b'>('\b')
                                                .map<'f'>('\f')
                                                .map<'n'>('\n')
                                                .map<'r'>('\r')
                                                .map<'t'>('\t');
    struct code_point_id {
        static constexpr auto rule = LEXY_LIT("u") >> dsl::code_unit_id<lexy::utf16_encoding, 4>;
        static constexpr auto value = lexy::construct<lexy::code_point>;
    };
    static constexpr auto rule = [] {
        auto code_point = -dsl::unicode::control;
        auto escape = dsl::backslash_escape.symbol<escaped_symbols>().rule(dsl::p<code_point_id>);
        return dsl::quoted(code_point, escape);
    }();
};

struct array {
    static constexpr auto rule = dsl::square_bracketed.opt_list(dsl::recurse<json_value>, dsl::sep(dsl::comma));
};

struct object {
    static constexpr auto rule = [] {
        auto item = dsl::p<string> + dsl::colon + dsl::recurse<json_value>;
        return dsl::curly_bracketed.opt_list(item, dsl::sep(dsl::comma));
    }();
};

struct json_value : lexy::transparent_production {
    static constexpr auto rule = dsl::p<null_> | dsl::p<true_> | dsl::p<false_> | dsl::p<number> | dsl::p<string> |
                                 dsl::p<object> | dsl::p<array>;
};

struct json {
    static constexpr auto max_recursion_depth = 1024;
    static constexpr auto whitespace = dsl::ascii::blank / dsl::ascii::newline;
    static constexpr auto rule = dsl::p<json_value> + dsl::eof;
};
}  // namespace grammar

using input_t = lexy::string_input<lexy::utf8_encoding>;

struct ctx_t {
    input_t input;
    lexy::parse_tree_for<input_t> tree;
};

static int validate(void *p) {
    auto *ctx = static_cast<ctx_t *>(p);
    return lexy::match<grammar::json>(ctx->input) ? 1 : 0;
}

static int build_tree(void *p) {
    auto *ctx = static_cast<ctx_t *>(p);
    // The tree object (and its memory) is reused across parses
    auto result = lexy::parse_as_tree<grammar::json>(ctx->tree, ctx->input, lexy::noop);
    return result.is_success() ? 1 : 0;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: lexy_bench <json_file> [--tree]\n");
        return 1;
    }
    char *data;
    size_t len = bench_read_file(argv[1], &data);
    ctx_t ctx{input_t(data, len), {}};
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
