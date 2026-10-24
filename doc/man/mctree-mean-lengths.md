% mctree-mean-lengths(1)
% Benjamin Redelings
% October 2026

# NAME

**mctree-mean-lengths** - Estimate branch and unresolved-node lengths for a query topology.

# SYNOPSIS

**mctree-mean-lengths** _TREE_ \[OPTIONS\] < _SAMPLES_

# DESCRIPTION

**mctree-mean-lengths** estimates lengths for a query topology from sampled trees. The query
may leave some relationships unresolved or leave some taxa's attachments unspecified. Only
sampled trees displaying the retained query partitions contribute to the estimates.
Results go to standard output; diagnostics go to standard error.

_TREE_ is the required positional query filename. It can contain an ordinary Newick tree
or an MC-tree description consisting of a Newick tree followed by partial partitions.
A branch defines a split of taxa into two sides. A full split places every taxon on one side
or the other; a partial split specifies the sides for only some taxa. For example:

    (A,B,C,D,E);
    A B | C D [ E ]

This requires a branch separating A and B from C and D, while leaving E's side unspecified.
Query preparation removes redundant partitions and can select a compatible subset when the
supplied partitions cannot coexist in the MC-tree representation. Matching uses this retained
query. **--verbose** reports partition-selection diagnostics and the number of matching samples.

Sampled trees are read from standard input in Newick or NEXUS format. They must supply branch
lengths and, after any pruning, have the same taxa as the query. **--skip** discards initial trees,
**--subsample** thins the remainder, and **--until** limits the number of retained trees examined.
The limit counts trees examined, not just those matching the query.

## Interpreting lengths

For each matching sample, a branch's length is assigned to the query branch it represents.
If it represents several query branches, its length is divided equally among them. For partial
queries, this correspondence accounts for taxa whose attachment is unspecified. Lengths of
branches resolving an unresolved query node are accumulated at that node instead.

A node length is therefore the total branch length within an unresolved region, not a node
age or the length of a single hidden branch. For example, if two sampled branches of lengths
4 and 6 resolve a fully unresolved (star) query, they contribute node length 10. Several sampled branches may
also contribute to one partial query branch.

Means summarize the complete per-tree contributions over matching samples, including zero
contributions. **--var** reports their standard deviations instead. Estimates are conditional
on the retained query being displayed, rather than averages over all sampled trees.

# OUTPUT

The first line is the ordinary part of the query as a Newick tree with estimated lengths.
Each partial branch then has a `branch LENGTH` line followed by its partition. Each unresolved
node with positive mean length has a `node LENGTH` line followed by a directed partition
identifying that node. This partition is an identifier; the preceding number is a node length,
not the length of the identifying branch. The format can be read by **draw-tree**.

With **--var**, the same records contain standard deviations. A node with constant positive
length is still printed, with deviation zero. **--no-node-lengths** suppresses node records.

# EXAMPLES

Estimate lengths for a query from sampled trees:

    mctree-mean-lengths query.tree < samples.trees

Discard 100 initial trees and report standard deviations:

    mctree-mean-lengths query.tree --skip=100 --var < samples.trees

# OPTIONS

**`-s`** _NUM_, **`--skip`** _NUM_
: Discard NUM initial tree samples before subsampling. The default is 0.

**`-x`** _NUM_, **`--subsample`** _NUM_
: Read every NUMth tree after skipping, starting with the first remaining tree. The default is 1.

**`-u`** _NUM_, **`--until`** _NUM_
: Examine at most NUM trees after skipping and subsampling. By default, read to the end.

**`--prune`** _TAXA_
: Remove the listed taxa from sampled trees before matching. Supply a comma-separated list
  or `@filename` containing names separated by commas or newlines. The query must already
  contain the desired remaining taxa.

**`--var`**
: Report standard deviations instead of means, using the empirical distribution of matching
  samples. A single matching sample gives deviation zero.

**`--no-node-lengths`**
: Omit node-length records. Branch estimates and the matching criterion are unchanged.

**`--drop-partial`**
: Remove partial query partitions before matching and estimating lengths. This can change
  which samples contribute; it does not merely hide partial-branch output.

**`--safe`**
: If trees were read but none matched, print the query topology without estimated lengths
  and exit successfully. Empty input and other failures remain errors.

**`--seed`** _NUM_
: Set the random seed used when selecting a compatible subset of query partitions.

**`-V`**, **`--verbose`**
: Show additional diagnostics on standard error.

**`-h`**, **`--help`**
: Display help and exit.

# SEE ALSO

**tree-mean-lengths**(1), **draw-tree**(1)

# REPORTING BUGS

BAli-Phy online help: <https://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
