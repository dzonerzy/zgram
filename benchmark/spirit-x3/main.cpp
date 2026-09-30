// Boost.Spirit X3 JSON benchmark. Same grammar as benchmark/rust-peg.
//
//   spirit_x3_bench file.json          validation only (no attribute)
//   spirit_x3_bench file.json --tree   builds zgram's tree: a flat array of
//                                      nodes (rule, span, subtree size,
//                                      child count) for value, object, pair,
//                                      array, string and number, rolled back
//                                      on backtracking; the same design as
//                                      zgram's, via a small custom directive.
#include <boost/spirit/home/x3.hpp>

#include <cstdint>
#include <cstring>
#include <functional>
#include <vector>

#include "harness.h"

namespace x3 = boost::spirit::x3;

#include "flat_tree.hpp"

// ── Grammar, defined once and instantiated twice: plain (validation) and
// with node[...] around the rules that make nodes (tree) ──

#define JSON_GRAMMAR(NS, N)                                                                              \
    namespace NS {                                                                                       \
    x3::rule<class value_r> const value = "value";                                                     \
    x3::rule<class object_r> const object = "object";                                                  \
    x3::rule<class pair_r> const pair = "pair";                                                        \
    x3::rule<class array_r> const array = "array";                                                     \
                                                                                                         \
    auto const ws = *x3::char_(" \t\n\r");                                                             \
    auto const hex = x3::char_("0-9a-fA-F");                                                           \
    auto const escape = x3::lit('\\') >> (x3::char_("\"\\\\/bfnrt") | (x3::lit('u') >> hex >> hex >> hex >> hex)); \
    auto const plain = ~x3::char_("\"\\\\");                                                           \
    auto const int_ = x3::lit('0') | (x3::char_("1-9") >> *x3::digit);                                \
    auto const string_ = N("string", x3::lit('"') >> *(escape | plain) >> x3::lit('"'));               \
    auto const number = N("number", -x3::lit('-') >> int_ >> -(x3::lit('.') >> +x3::digit) >>          \
                                        -(x3::char_("eE") >> -x3::char_("+-") >> +x3::digit));          \
                                                                                                         \
    auto const value_def =                                                                               \
        N("value", object | array | string_ | number | x3::lit("true") | x3::lit("false") | x3::lit("null")); \
    auto const object_def = N("object", (x3::lit('{') >> ws >> x3::lit('}')) |                          \
                                            (x3::lit('{') >> ws >> pair >> *(x3::lit(',') >> ws >> pair) >> ws >> x3::lit('}'))); \
    auto const pair_def = N("pair", ws >> string_ >> ws >> x3::lit(':') >> ws >> value >> ws);         \
    auto const array_def = N("array", (x3::lit('[') >> ws >> x3::lit(']')) |                            \
                                          (x3::lit('[') >> ws >> value >> *(x3::lit(',') >> ws >> value) >> ws >> x3::lit(']'))); \
                                                                                                         \
    BOOST_SPIRIT_DEFINE(value, object, pair, array)                                                    \
                                                                                                         \
    auto const json = ws >> value >> ws >> x3::eoi;                                                    \
    }

#define PLAIN_RULE(name, p) (p)
#define NODE_RULE(name, p) node(name)[p]

JSON_GRAMMAR(plain_grammar, PLAIN_RULE)
JSON_GRAMMAR(tree_grammar, NODE_RULE)

struct ctx_t {
    const char *begin;
    const char *end;
    Tree tree;
};

static int validate(void *p) {
    auto *ctx = static_cast<ctx_t *>(p);
    const char *first = ctx->begin;
    return x3::parse(first, ctx->end, plain_grammar::json) ? 1 : 0;
}

static int build_tree(void *p) {
    auto *ctx = static_cast<ctx_t *>(p);
    ctx->tree.nodes.clear();  // the node buffer is reused across parses, as in zgram_bench
    const char *first = ctx->begin;
    return x3::parse(first, ctx->end, x3::with<tree_tag>(std::ref(ctx->tree))[tree_grammar::json]) ? 1 : 0;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: spirit_x3_bench <json_file> [--tree]\n");
        return 1;
    }
    char *data;
    size_t len = bench_read_file(argv[1], &data);
    ctx_t ctx{data, data + len, {}};
    ctx.tree.base = data;
    if (argc > 2 && std::strcmp(argv[2], "--tree") == 0) {
        build_tree(&ctx);
        fprintf(stderr, "Tree nodes: %zu\n", ctx.tree.nodes.size());
        bench_run("Spirit X3 tree", len, build_tree, &ctx);
    } else {
        bench_run("Spirit X3", len, validate, &ctx);
    }
    return 0;
}
