% trees-pair-distances(1)
% Benjamin Redelings
% October 2026

# NAME

**trees-pair-distances** - Summarize leaf-to-leaf distances across sampled trees.

# SYNOPSIS

**trees-pair-distances** [OPTIONS] **<** _SAMPLED-TREES_

# DESCRIPTION

For each pair of leaves, compute the distance between them in each selected
tree and report the mean across trees. **--var** reports the population standard
deviation instead. This compares leaves within trees, not distances between
different tree topologies.

The default distance is the sum of branch lengths on the path between two
leaves. **--RF** instead counts edges on that path. Despite the option name,
this is not the Robinson-Foulds distance between trees.

# INPUT AND SAMPLE SELECTION

Read a Newick or NEXUS tree sample from standard input. Newick samples contain
one tree per line. Trees must have the same leaf-label set; their input leaf
orders may differ. For branch-length distances, supply trees with branch lengths.

Selection proceeds in this order:

1. Discard **--skip** initial trees (default 0).
2. Retain the next tree, then every **--subsample**-th tree (default 1).
3. Process at most **--max** retained trees, if a maximum is supplied.

For example, **--skip 1 --subsample 2 --max 3** selects input trees 2, 4, and 6,
if available. A final selected tree is retained even if advancing to the next
selection reaches EOF. Use a nonnegative skip and maximum and a positive
subsampling factor. Selecting no trees is an error, including **--max 0**.

Trees are interpreted as unrooted for distance calculation. An artificial
degree-two root is suppressed, joining its incident branches. This preserves
branch-length distances but affects edge counts: the root does not contribute
an extra edge to a path. Other degree-two nodes are not automatically removed.

# OUTPUT

Write a tab-separated matrix to standard output. The first line lists leaf
names in the reader's leaf order; the following rows use that same order.
There are no row labels. The matrix is symmetric, with zero diagonal entries.

Each mean is the sum of the selected distances divided by the number of selected
trees. Each standard deviation uses that same denominator, not the sample
correction of one fewer tree. With one selected tree, standard deviations are
zero. The calculation uses the difference between the second moment and the
squared mean; nearly constant distances can be sensitive to floating-point
roundoff.

# OPTIONS

**-h**, **--help**
: Print help and exit.

**--skip** _N_
: Discard _N_ initial trees. Default: 0.

**--subsample** _N_
: Retain every _N_-th tree after skipping. Default: 1.

**--max** _N_
: Process at most _N_ selected trees. By default, process all selected trees.

**--RF**
: Count path edges instead of summing branch lengths.

**--var**
: Report population standard deviations instead of means. The output is a
  standard deviation, not a variance. May be combined with **--RF**.

# EXAMPLES

For `sample.trees` containing:

```text
(A:1,B:1,C:1);
(A:3,B:3,C:3);
```

Compute mean distances or their standard deviations:

```sh
trees-pair-distances < sample.trees
trees-pair-distances --var < sample.trees
```

The off-diagonal means are 4 and the standard deviations are 2. Edge counts are
2 for every off-diagonal entry in both trees:

```sh
trees-pair-distances --RF < sample.trees
trees-pair-distances --max 1 < sample.trees
trees-pair-distances --skip 1 --subsample 2 --max 3 < sample.trees
```

# SEE ALSO

**tree-tool**(1), **trees-distances**(1), **tree-mean-lengths**(1)

# REPORTING BUGS

BAli-Phy online help: <https://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
