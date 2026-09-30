// Boost.Spirit X3 typed-AST JSON benchmark: the idiomatic X3 way of parsing
// JSON into values: an x3::variant of null, bool, double, string, array
// (std::vector) and object (std::map), built by attribute propagation, with
// escapes decoded (\uXXXX as UTF-8).
#include <boost/fusion/include/std_pair.hpp>
#include <boost/spirit/home/x3.hpp>
#include <boost/spirit/home/x3/support/ast/variant.hpp>

#include <map>
#include <string>
#include <vector>

#include "harness.h"

namespace x3 = boost::spirit::x3;

namespace ast {
struct null_t {};
struct value;
using array = std::vector<value>;
using object = std::map<std::string, value>;
struct value : x3::variant<null_t, bool, double, std::string, x3::forward_ast<array>, x3::forward_ast<object>> {
    using base_type::base_type;
    using base_type::operator=;
};
}  // namespace ast

namespace grammar {
x3::rule<class value_r, ast::value> const value = "value";
x3::rule<class array_r, ast::array> const array = "array";
x3::rule<class object_r, ast::object> const object = "object";
x3::rule<class member_r, std::pair<std::string, ast::value>> const member = "member";
x3::rule<class string_r, std::string> const string_ = "string";

struct escapes_ : x3::symbols<char> {
    escapes_() { add("\"", '"')("\\", '\\')("/", '/')("b", '\b')("f", '\f')("n", '\n')("r", '\r')("t", '\t'); }
} const escapes;

auto const push_char = [](auto &ctx) { x3::_val(ctx) += x3::_attr(ctx); };
auto const push_utf8 = [](auto &ctx) {
    unsigned cp = x3::_attr(ctx);
    std::string &s = x3::_val(ctx);
    if (cp < 0x80) {
        s += char(cp);
    } else if (cp < 0x800) {
        s += char(0xC0 | (cp >> 6));
        s += char(0x80 | (cp & 0x3F));
    } else {
        s += char(0xE0 | (cp >> 12));
        s += char(0x80 | ((cp >> 6) & 0x3F));
        s += char(0x80 | (cp & 0x3F));
    }
};
auto const hex4 = x3::uint_parser<unsigned, 16, 4, 4>();

auto const string__def = x3::lexeme['"' >> *((x3::lit('\\') >> (escapes[push_char] | (x3::lit('u') >> hex4[push_utf8]))) |
                                             (~x3::char_("\"\\\\"))[push_char]) >>
                                    '"'];
auto const value_def = (x3::lit("null") >> x3::attr(ast::null_t{})) | x3::bool_ | x3::double_ | string_ | array | object;
auto const array_def = '[' >> -(value % ',') >> ']';
auto const member_def = string_ >> ':' >> value;
auto const object_def = '{' >> -(member % ',') >> '}';

BOOST_SPIRIT_DEFINE(value, array, object, member, string_)
}  // namespace grammar

struct ctx_t {
    const char *begin;
    const char *end;
};

static int parse_ast(void *p) {
    auto *ctx = static_cast<ctx_t *>(p);
    const char *first = ctx->begin;
    ast::value result;
    bool ok = x3::phrase_parse(first, ctx->end, grammar::value >> x3::eoi, x3::char_(" \t\n\r"), result);
    BENCH_KEEP(&result);
    return ok ? 1 : 0;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: spirit_x3_ast_bench <json_file>\n");
        return 1;
    }
    char *data;
    size_t len = bench_read_file(argv[1], &data);
    ctx_t ctx{data, data + len};
    bench_run("Spirit X3 typed AST", len, parse_ast, &ctx);
    return 0;
}
