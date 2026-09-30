// Boost.Spirit X3 expression benchmark. Same grammar as zgram's expression
// benchmark (call is tried before ident, so identifiers backtrack).
//
//   spirit_x3_expr_bench file.txt          validation only
//   spirit_x3_expr_bench file.txt --tree   zgram's tree as a flat node array
#include <cstring>

#include "flat_tree.hpp"
#include "harness.h"

#define EXPR_GRAMMAR(NS, N)                                                                               \
    namespace NS {                                                                                        \
    x3::rule<class sum_r> const sum = "sum";                                                            \
    x3::rule<class power_r> const power = "power";                                                      \
    x3::rule<class unary_r> const unary = "unary";                                                      \
                                                                                                          \
    auto const ws = *x3::char_(" \t\n\r");                                                              \
    auto const number = N("number", +x3::digit >> -(x3::lit('.') >> +x3::digit) >>                      \
                                        -(x3::char_("eE") >> -x3::char_("+-") >> +x3::digit));           \
    auto const ident = N("ident", x3::char_("a-zA-Z_") >> *x3::char_("a-zA-Z0-9_"));                    \
    auto const args = sum >> *(ws >> x3::lit(',') >> ws >> sum);                                        \
    auto const call = N("call", ident >> ws >> x3::lit('(') >> ws >> -args >> ws >> x3::lit(')'));      \
    auto const primary = number | call | ident | (x3::lit('(') >> ws >> sum >> ws >> x3::lit(')'));    \
    auto const product = N("product", power >> *(ws >> x3::char_("*/%") >> ws >> power));               \
                                                                                                          \
    auto const sum_def = N("sum", product >> *(ws >> x3::char_("+-") >> ws >> product));                \
    auto const power_def = N("power", unary >> -(ws >> x3::lit('^') >> ws >> power));                   \
    auto const unary_def = N("unary", (x3::lit('-') >> ws >> unary) | primary);                         \
    BOOST_SPIRIT_DEFINE(sum, power, unary)                                                              \
                                                                                                          \
    auto const expr = N("expr", ws >> sum >> ws) >> x3::eoi;                                            \
    }

#define PLAIN_RULE(name, p) (p)
#define NODE_RULE(name, p) node(name)[p]

EXPR_GRAMMAR(plain_grammar, PLAIN_RULE)
EXPR_GRAMMAR(tree_grammar, NODE_RULE)

struct ctx_t {
    const char *begin;
    const char *end;
    Tree tree;
};

static int validate(void *p) {
    auto *ctx = static_cast<ctx_t *>(p);
    const char *first = ctx->begin;
    return x3::parse(first, ctx->end, plain_grammar::expr) ? 1 : 0;
}

static int build_tree(void *p) {
    auto *ctx = static_cast<ctx_t *>(p);
    ctx->tree.nodes.clear();
    const char *first = ctx->begin;
    return x3::parse(first, ctx->end, x3::with<tree_tag>(std::ref(ctx->tree))[tree_grammar::expr]) ? 1 : 0;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: spirit_x3_expr_bench <expr_file> [--tree]\n");
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
