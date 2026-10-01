#include "tree/sequencetree.hh"

#include <cstdlib>
#include <iostream>

// Keep checks active in release builds, where tree assertions are disabled.
void require(bool condition, const char* message)
{
    if (condition) return;
    std::cerr << message << '\n';
    std::exit(1);
}

// Exercise public tree operations not covered by the command-line tests. These
// cases can move to tool tests if those tools expose the same operations and results.
int main()
{
    // Quote runs must retain taxon identity; ordinary single-apostrophe labels miss this case.
    for (const std::string label: {"'A", "A'", "A''B", "'''", "A_B C"})
        for (auto underscores: {Underscore::literal, Underscore::blank})
        {
            auto quoted = escape_for_newick(label, underscores);
            require(unescape_from_newick(quoted, underscores) == label, "Quoted label round-trip");
            SequenceTree tree;
            tree.parse("(" + quoted + ",other);");
            require(tree.get_label(0) == label, "Quoted label parsing");
        }

    // Different resolutions must yield a symmetric distance, regardless of array sizes.
    SequenceTree star, resolved;
    star.parse("(A:1,B:1,C:1,D:1);");
    resolved.parse("((A:1,B:1):2,C:1,D:1);");
    require(branch_distance(star, resolved) == 2, "Star-to-resolved distance");
    require(branch_distance(resolved, star) == 2, "Resolved-to-star distance");
    require(branch_distance(resolved, resolved) == 0, "Identical-tree distance");

    // Cover root placement and ancestor arguments, which the three-leaf tool case cannot express.
    RootedSequenceTree rooted;
    rooted.parse("((A,B)X,(C,D)Y)R;");
    int x = rooted.index("X"), r = rooted.index("R");
    require(rooted.common_ancestor(rooted.index("A"), rooted.index("B")) == x, "Sibling ancestor");
    require(rooted.common_ancestor(rooted.index("A"), rooted.index("C")) == r, "Root ancestor");
    require(rooted.common_ancestor(x, rooted.index("A")) == x, "Ancestor argument");
    require(rooted.common_ancestor(x, x) == x, "Identical ancestor arguments");
    require(rooted.common_ancestor(rooted.index("A"), r) == r, "Root argument");
    rooted.reroot(rooted.index("A"));
    require(rooted.common_ancestor(rooted.index("B"), rooted.index("C")) == x, "Leaf root ancestor");
    rooted.parse("single;");
    require(rooted.common_ancestor(0, 0) == 0, "Singleton ancestor");

    // Joining goes through a virtual base; ordinary copy tests do not cover its initialization.
    RootedSequenceTree left, right;
    left.parse("(A:1,B:2);");
    right.parse("(C:3,D:4);");
    RootedSequenceTree joined(left, right);
    require(joined.get_leaf_labels() == std::vector<std::string>({"A", "B", "C", "D"}), "Joined labels");
    require(joined.root().degree() == 4 and tree_length(joined) == 10, "Joined root and lengths");

    // Splitting preserves total length, including reverse orientations and absent lengths;
    // rerooting tools overwrite the new lengths and would conceal this regression.
    for (bool lengths: {false, true})
        for (int b: {0, star.n_branches()})
        {
            SequenceTree split;
            split.parse(lengths ? "(A:1,B:1,C:1,D:1);" : "(A,B,C,D);");
            int inserted = split.create_node_on_branch(b);
            for (int edge = 0; edge < split.n_branches(); ++edge)
                require(split.branch(edge).has_length() == lengths, "Split length presence");
            if (lengths) require(tree_length(split) == 4, "Split total length");
            split.remove_node_from_branch(inserted);
            if (lengths) require(branch_distance(split, star) == 0, "Split/remove round-trip");
        }
}
