// Flat parse tree for Spirit X3: node("name")[p] records a node spanning p's
// match in one contiguous array (rule, span, subtree size, child count),
// rolled back on backtracking, the same design as zgram's node buffer.
#pragma once

#include <boost/spirit/home/x3.hpp>

#include <cstdint>
#include <functional>
#include <vector>

namespace x3 = boost::spirit::x3;

// ── Flat parse tree ──

struct FlatNode {
    const char *rule;
    uint32_t start, end, subtree_size, child_count;
};

struct Tree {
    const char *base = nullptr;
    std::vector<FlatNode> nodes;
};

struct tree_tag;

// node("name")[p]: record a node spanning p's match
template <typename Subject>
struct node_parser : x3::unary_parser<Subject, node_parser<Subject>> {
    using base_type = x3::unary_parser<Subject, node_parser<Subject>>;
    static bool const has_attribute = false;
    using attribute_type = x3::unused_type;

    const char *name;
    constexpr node_parser(Subject const &subject, const char *name) : base_type(subject), name(name) {}

    template <typename It, typename Ctx, typename RCtx, typename Attr>
    bool parse(It &first, It const &last, Ctx const &ctx, RCtx &rctx, Attr &) const {
        Tree &tree = x3::get<tree_tag>(ctx).get();
        const size_t idx = tree.nodes.size();
        const It start = first;
        tree.nodes.emplace_back();
        if (!this->subject.parse(first, last, ctx, rctx, x3::unused)) {
            tree.nodes.resize(idx);  // drop this node and any children
            first = start;
            return false;
        }
        uint32_t kids = 0;
        for (size_t i = idx + 1; i < tree.nodes.size(); i += tree.nodes[i].subtree_size + 1) ++kids;
        tree.nodes[idx] = {name, uint32_t(start - tree.base), uint32_t(first - tree.base),
                           uint32_t(tree.nodes.size() - idx - 1), kids};
        return true;
    }
};

struct node_gen {
    const char *name;
    template <typename Subject>
    constexpr node_parser<typename x3::extension::as_parser<Subject>::value_type> operator[](Subject const &s) const {
        return {x3::as_parser(s), name};
    }
};
constexpr node_gen node(const char *name) { return {name}; }

