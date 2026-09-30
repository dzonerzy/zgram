// PEGTL expression benchmark: validation (parse without actions) and its
// built-in parse_tree with the same node rules as zgram. Same grammar as
// zgram's expression benchmark.
#include <cstring>
#include <string>

#include <tao/pegtl.hpp>
#include <tao/pegtl/contrib/parse_tree.hpp>

#include "harness.h"

namespace pegtl = tao::pegtl;

namespace grammar {
using namespace pegtl;
struct ws : star<one<' ', '\t', '\n', '\r'>> {};
struct sum;
struct power;
struct number : seq<plus<digit>, opt<one<'.'>, plus<digit>>, opt<one<'e', 'E'>, opt<one<'+', '-'>>, plus<digit>>> {};
struct ident : identifier {};
struct args : seq<sum, star<ws, one<','>, ws, sum>> {};
struct call : seq<ident, ws, one<'('>, ws, opt<args>, ws, one<')'>> {};
struct primary : sor<number, call, ident, seq<one<'('>, ws, sum, ws, one<')'>>> {};
struct unary : sor<seq<one<'-'>, ws, unary>, primary> {};
struct power : seq<unary, opt<ws, one<'^'>, ws, power>> {};
struct product : seq<power, star<ws, one<'*', '/', '%'>, ws, power>> {};
struct sum : seq<product, star<ws, one<'+', '-'>, ws, product>> {};
struct expr : seq<ws, sum, ws> {};
struct text : seq<expr, eof> {};
}  // namespace grammar

template <typename Rule>
using tree_selector = pegtl::parse_tree::selector<
    Rule, pegtl::parse_tree::store_content::on<grammar::expr, grammar::sum, grammar::product, grammar::power,
                                                grammar::unary, grammar::call, grammar::number, grammar::ident>>;

static size_t count_nodes(const pegtl::parse_tree::node &n) {
    size_t total = n.is_root() ? 0 : 1;
    for (const auto &c : n.children) total += count_nodes(*c);
    return total;
}

struct ctx_t {
    const char *data;
    size_t len;
};

static int validate(void *p) {
    auto *ctx = static_cast<ctx_t *>(p);
    pegtl::memory_input in(ctx->data, ctx->len, "expr");
    return pegtl::parse<grammar::text>(in) ? 1 : 0;
}

static int build_tree(void *p) {
    auto *ctx = static_cast<ctx_t *>(p);
    pegtl::memory_input in(ctx->data, ctx->len, "expr");
    auto root = pegtl::parse_tree::parse<grammar::text, tree_selector>(in);
    BENCH_KEEP(root.get());
    return root ? 1 : 0;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: pegtl_expr_bench <expr_file> [--tree]\n");
        return 1;
    }
    char *data;
    size_t len = bench_read_file(argv[1], &data);
    ctx_t ctx{data, len};
    if (argc > 2 && std::strcmp(argv[2], "--tree") == 0) {
        pegtl::memory_input in(data, len, "expr");
        auto root = pegtl::parse_tree::parse<grammar::text, tree_selector>(in);
        if (root) fprintf(stderr, "Tree nodes: %zu\n", count_nodes(*root));
        bench_run("PEGTL tree", len, build_tree, &ctx);
    } else {
        bench_run("PEGTL", len, validate, &ctx);
    }
    return 0;
}
