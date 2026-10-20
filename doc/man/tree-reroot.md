% tree-reroot(1)
% Benjamin Redelings
% October 2026

# NAME

**tree-reroot** - Place the root at a selected node or branch.

# SYNOPSIS

**tree-reroot** _TREE_ **--outgroup** _NAME_

**tree-reroot** _TREE_ **--taxa** _LIST_

**tree-reroot** _TREE_ **--leaf** _NAME_

**tree-reroot** _TREE_ **--parent-of** _NAME_

**tree-reroot** _TREE_ **--branch** _SPLIT_

# DESCRIPTION

Change the root of the first tree in a file and write one Newick tree to
standard output. Choose an existing node with **--taxa**, **--leaf**, or
**--parent-of**, or insert a root halfway along a branch with **--outgroup** or
**--branch**.

Rerooting changes the orientation used to write the tree. It does not prune
leaves or automatically suppress the former root if that node has degree two.
Splitting a branch replaces its length with two equal halves, preserving
leaf-to-leaf distances. The branch-splitting modes require branch lengths.

At least one selector is required. If several are supplied, the first present
in this list wins: **--outgroup**, **--taxa**, **--leaf**, **--parent-of**,
**--branch**. The other selectors are ignored.

# INPUT

_TREE_ is a positional Newick or NEXUS filename; only its first tree is used.
**-** is not a standard-input alias.
Names must match the tree's taxon labels.

# OPTIONS

**-h**, **--help**
: Print help and exit.

**--outgroup** _NAME_
: Insert the root halfway along the branch leading to this leaf. The outgroup
  is supplied with this option, not as a second positional argument.

**--taxa** _LIST_
: Root at the node where the paths connecting three specified taxa meet.
  Supply exactly three names, separated by commas, or use **@filename** to read
  names separated by commas or newlines. Empty entries are ignored. Prefix an
  initial literal `@` with a backslash to avoid reading a file.

**--leaf** _NAME_
: Use this leaf node itself as the root. This differs from inserting a root
  along its branch and can produce a Newick tree with a named degree-one root.

**--parent-of** _NAME_
: Root at the node adjacent to this leaf. The selected node does not depend on
  the current orientation of the tree.

**--branch** _SPLIT_
: Insert the root halfway along the branch matching a partition. Supply a
  quoted, space-separated expression such as **'A B | C D'**, with **|** as a
  separate token. The expression may specify a partial partition, but it must
  identify a matching branch. This syntax cannot represent names containing
  spaces. The requested and matched partitions are also printed to standard
  error.

# EXAMPLES

For a file `tree.nwk` containing `((A:1,B:1):2,C:1,D:1);`:

```sh
tree-reroot tree.nwk --outgroup D > rooted.nwk
tree-reroot tree.nwk --taxa A,C,D
tree-reroot tree.nwk --parent-of A
tree-reroot tree.nwk --leaf D
tree-reroot tree.nwk --branch 'A B | C D'
```

# SEE ALSO

**tree-tool**(1), **tree-partitions**(1)

# REPORTING BUGS

BAli-Phy online help: <https://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
