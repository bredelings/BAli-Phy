% tree-partitions(1)
% Benjamin Redelings
% October 2026

# NAME

**tree-partitions** - Print branch partitions or information about one tree.

# SYNOPSIS

**tree-partitions** _TREE_ [OPTIONS]

# DESCRIPTION

Print the splits of the leaf set induced by the internal branches of a tree.
Cutting a branch divides the leaves into two groups; these groups describe its
partition. With **--all**, include leaf branches, which separate one leaf from
all the others.

Alternatively, **--info** prints structural information, and **--scale** writes
a tree with scaled branch lengths. These modes are selected in that order:
**--info** takes precedence over **--scale**, which takes precedence over
partition output. **--all** affects only partition output.

# INPUT

_TREE_ is a Newick or NEXUS file. Only its first tree is used. The filename may
also be supplied with **--tree**. This command requires a file; **-** is not a
standard-input alias.

# OUTPUT

Partitions are written to standard output, with spaces between leaf names, a
**|** between the two groups, and a blank line between partitions. For example,
`A B | C D` separates A and B from C and D. The two sides describe an unordered
split: swapping them does not change the partition.

Output is per branch, not per distinct split. Degree-two nodes can therefore
produce repeated splits, and an internal branch can separate a single leaf
when a degree-two node lies along its path. The command does not suppress these
nodes or combine repeated partitions.

Information output includes node and branch counts, the label count, counts of
degree-two and degree-four-or-higher nodes, and whether the tree is a simple
bifurcating or multifurcating tree. These are undirected node degrees; an
artificial degree-two Newick root contributes to the degree-two count.

Scaling output is one Newick tree. Leaf ordering is standardized before output;
the command does not preserve the input's Newick layout or designated root as
a rooted representation.

# OPTIONS

**-h**, **--help**
: Print help and exit.

**--tree** _TREE_
: Supply the input filename instead of the positional argument.

**--all**
: Include partitions from leaf branches as well as internal branches.

**--info**
: Print structural information instead of partitions.

**--scale** _FACTOR_
: Multiply each branch length by _FACTOR_ and print the resulting tree.
  Supply a tree with branch lengths for this mode.

# EXAMPLES

For a file `tree.nwk` containing `((A:1,B:1):2,C:1,D:1);`:

```sh
tree-partitions tree.nwk
tree-partitions tree.nwk --all
tree-partitions tree.nwk --info
tree-partitions tree.nwk --scale 0.5 > scaled.nwk
```

# SEE ALSO

**tree-tool**(1), **tree-reroot**(1), **trees-consensus**(1)

# REPORTING BUGS

BAli-Phy online help: <https://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
