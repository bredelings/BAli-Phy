#include "tree/sequencetree.hh"
#include "tools/read-trees.hh"
#include "tools/tree-dist.hh"
#include <sstream>
#include "tools/distance-methods.hh"

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
    // Protect the shared burn-in parser and its reusable fraction output; tool summaries cannot
    // detect stale parser state. Retire if burn-in parsing moves into a directly tested CLI parser.
    {
        std::optional<double> fraction;
        require(parse_tree_burnin("10%", fraction) == 0 and fraction == .1, "Percentage burn-in");
        require(parse_tree_burnin("12", fraction) == 12 and not fraction, "Integer clears percentage");
        require(parse_tree_burnin("0%", fraction) == 0 and fraction == 0, "Zero percentage stays engaged");
        require(parse_tree_burnin("0", fraction) == 0 and not fraction, "Zero integer burn-in");
        require(parse_tree_burnin("100%", fraction) == 0 and fraction == 1, "Percentage upper bound");
        for (const auto* text: {"invalid", "-1%", "101%"})
        {
            bool rejected = false;
            try { parse_tree_burnin(text, fraction); }
            catch (const std::exception&) { rejected = true; }
            require(rejected, "Reject malformed or out-of-range burn-in");
        }
    }

    // Protect common burn-in and selected tree identities, which tool summaries cannot expose.
    // Integer loading is the independent reference for selection order; retire this group if
    // both loading paths are replaced by one tested selection implementation.
    {
        // Encode original ordinals in a pendant length so thinning mistakes remain observable.
        auto sample = [](int n, int skip=0, std::optional<int> last={}, int stride=1,
                         std::optional<int> max={})
        {
            std::ostringstream text;
            for (int i=1; i<=n; ++i) text<<"((A:"<<i<<",B:1):1,C:1,D:1);\n";
            std::istringstream input(text.str());
            return tree_sample(input, skip, last, stride, max);
        };
        for (auto last: {std::optional<int>{}, std::optional<int>{30}})
            for (int stride: {1, 3})
                for (auto max: {std::optional<int>{}, std::optional<int>{1},
                                std::optional<int>{5}, std::optional<int>{20}})
                {
                    std::vector<tree_sample> runs{sample(40,0,last), sample(100,0,last)};
                    select_after_burnin(runs, .1, stride, max);
                    const int skip = last ? 3 : 4;
                    for (int r=0; r<2; ++r)
                    {
                        auto expected = sample(r == 0 ? 40 : 100, skip, last, stride, max);
                        require(runs[r].size() == expected.size(), "Selected run size");
                        for (int i=0; i<expected.size(); ++i)
                            require(runs[r][i].branch_lengths[0] == expected[i].branch_lengths[0],
                                    "Percentage and integer burn-in select the same original trees");
                    }
                }
        std::vector<tree_sample> runs{sample(10), sample(100)};
        select_after_burnin(runs, .1, 1, {});
        require(runs[0].size() == 9 and runs[1].size() == 99, "Common burn-in count");
        require(runs[0][0].branch_lengths[0] == 2 and runs[1][0].branch_lengths[0] == 2,
                "Both runs start after the same burn-in");
        runs = {sample(1), sample(100)};
        select_after_burnin(runs, .1, 1, {});
        require(runs[0].size() == 1 and runs[1].size() == 100, "Preserve the shortest run");
        runs = {sample(100)};
        select_after_burnin(runs, .1, 1, 20);
        require(runs[0].size() == 20, "Cap after burn-in");
        require(runs[0][0].branch_lengths[0] == 11 and runs[0][19].branch_lengths[0] == 100,
                "Maximum-size thinning preserves survivor endpoints");
        // Loading another capped file must not resample trees already accumulated.
        auto combined = sample(4);
        std::istringstream extra("((A:5,B:1):1,C:1,D:1);\n((A:6,B:1):1,C:1,D:1);\n");
        combined.load_file(extra, 0, {}, 1, 1);
        require(combined.size() == 5 and combined[0].branch_lengths[0] == 1
                and combined[4].branch_lengths[0] == 5, "Cap only the appended run");
    }

    // Reader boundaries and adapter order must retain the last selected tree. Tool tests
    // cannot inspect exhaustion directly; retire this group if this reader API is replaced.
    for (bool rooted_input: {false, true})
    for (bool nexus: {false, true})
        for (int count: {0, 1, 5, 6})
            for (int limit: {0, 1, 3, 8})
                for (bool limit_first: {false, true})
                {
                    std::ostringstream text;
                    if (nexus) text << "#NEXUS\nbegin trees;\n";
                    for (int i = 1; i <= count; ++i)
                    {
                        if (nexus) text << "tree t" << i << " = ";
                        text << "(A:" << i << ",B:1,C:1);\n";
                    }
                    if (nexus) text << "end;\n";
                    std::istringstream input(text.str());
                    using namespace trees_format;
                    std::shared_ptr<reader_t> reader = std::make_shared<Newick_or_NEXUS>(input);
                    if (limit_first) reader = std::make_shared<Last>(limit, *reader);
                    reader = std::make_shared<Subsample>(2, *reader);
                    if (not limit_first) reader = std::make_shared<Last>(limit, *reader);
                    int obtained = 0;
                    RootedSequenceTree tree;
                    while (rooted_input ? reader->next_tree(tree) : reader->next_tree(static_cast<Tree&>(tree)))
                    {
                        require(tree.branch(tree.index("A")).length() == 1 + 2*obtained,
                                "Selected tree identity");
                        ++obtained;
                    }
                    int expected = limit_first ? (std::min(count, limit) + 1)/2
                                               : std::min((count + 1)/2, limit);
                    require(obtained == expected, "Selected tree count");
                    require(reader->done(), "Reader exhaustion");
                    require(not reader->next_tree(tree), "Repeated exhausted read");
                    require(not input.bad(), "Normal exhaustion is not a parse error");
                }

    // A singleton must use the same representation whether constructed or parsed;
    // usual tree-tool inputs have several taxa and do not exercise its first edge.
    Tree singleton;
    singleton.add_first_node();
    require(singleton.node(0).name() == 0 and singleton.n_branches() == 0, "Singleton indices");
    Tree copied(singleton);
    require(copied.n_nodes() == 1 and copied.leaf_branches().empty(), "Singleton copy");
    require(singleton.add_leaf_node(0).name() == 1, "Second node index");
    require(singleton.n_leaves() == 2 and singleton.n_branches() == 1, "First edge counts");
    singleton.branch(0).set_length(3);
    require(singleton.distance(0, 1) == 3, "First edge length");
    Matrix distances(2, 2, 0.0);
    distances(0, 1) = distances(1, 0) = 3;
    require(tree_length(NJ(distances)) == 3, "Two-taxon neighbor joining");

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

    // Splitting a polytomy exercises temporary leaf insertion followed by reconnection.
    SequenceTree partitioned(star);
    int new_branch = partitioned.induce_partition(boost::dynamic_bitset<>(4, 3));
    auto partition = branch_partition(partitioned, new_branch);
    require(partition == boost::dynamic_bitset<>(4, 3) or ~partition == boost::dynamic_bitset<>(4, 3),
            "Induced partition");
    require(partitioned.n_leaves() == 4, "Leaf count after reconnection");

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

    // Failed replacements must preserve structure, metadata, and roots, and release partial trees.
    // Tool error tests cannot inspect the same object after catching a parse failure.
    rooted.parse("((A[&tag=old]:1,B:2):3,C:4)[&root=yes];");
    const auto original = rooted.write();
    const auto original_root = rooted.root().name();
    for (const std::string input: {"(A[&new=x],B,D);", "((A[&new=x],B,C);", "(A:bad,B,C);"})
    {
        bool failed = false;
        try { rooted.parse_with_names(input, {"A", "B", "C"}, Underscore::literal); }
        catch (const std::exception&) { failed = true; }
        require(failed, "Invalid replacement accepted");
        require(rooted.write() == original and rooted.root().name() == original_root, "Failed parse changed tree");
    }
    rooted.parse_with_names("(A:2,B:3,C:4);", {"A", "B", "C"}, Underscore::literal);
    require(tree_length(rooted) == 9, "Successful parse after failures");

    // Pruning must preserve the survivor itself as well as labels and index mappings;
    // normal multi-taxon summaries do not reach this ownership boundary.
    for (const std::string input: {"(A:1,B:2,C:3);", "((A:1,B:2):3,(C:4,D:5):6);"})
    {
        SequenceTree original_tree;
        original_tree.parse(input);
        for (int kept = 0; kept < original_tree.n_leaves(); ++kept)
        {
            SequenceTree pruned(original_tree);
            const BranchNode* survivor = pruned.node(kept);
            std::vector<int> removed;
            for (int n = 0; n < pruned.n_leaves(); ++n)
                if (n != kept) removed.push_back(n);
            auto mapping = pruned.prune_leaves(removed);
            require(mapping == std::vector<int>{kept}, "Pruned singleton mapping");
            require(pruned.n_nodes() == 1 and pruned.n_branches() == 0, "Pruned singleton counts");
            require(pruned.node(0) == survivor, "Pruning replaced survivor");
            require(pruned.get_label(0) == original_tree.get_label(kept), "Pruned label");
            SequenceTree copy(pruned);
        }
    }
    for (int removed: {0, 1})
    {
        RootedSequenceTree pair;
        pair.parse("(A[&tag=first]:1,B[&tag=second]:2);");
        pair.remove_node_from_branch(pair.root());
        pair.reroot(1-removed);
        const BranchNode* survivor = pair.root();
        auto mapping = pair.prune_leaf(removed);
        require(mapping == std::vector<int>{1-removed}, "Single-leaf pruning mapping");
        require(pair.root() == survivor and pair.root().name() == 0, "Pruned root identity");
        require(pair.write().find(removed ? "first" : "second") != std::string::npos, "Pruned attributes");
        RootedSequenceTree copy(pair);
    }

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
