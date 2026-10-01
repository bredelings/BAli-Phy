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
    // Different resolutions must yield a symmetric distance, regardless of array sizes.
    SequenceTree star, resolved;
    star.parse("(A:1,B:1,C:1,D:1);");
    resolved.parse("((A:1,B:1):2,C:1,D:1);");
    require(branch_distance(star, resolved) == 2, "Star-to-resolved distance");
    require(branch_distance(resolved, star) == 2, "Resolved-to-star distance");
    require(branch_distance(resolved, resolved) == 0, "Identical-tree distance");

    // Joining goes through a virtual base; ordinary copy tests do not cover its initialization.
    RootedSequenceTree left, right;
    left.parse("(A:1,B:2);");
    right.parse("(C:3,D:4);");
    RootedSequenceTree joined(left, right);
    require(joined.get_leaf_labels() == std::vector<std::string>({"A", "B", "C", "D"}), "Joined labels");
    require(joined.root().degree() == 4 and tree_length(joined) == 10, "Joined root and lengths");
}
