% tree-tool(1)
% Benjamin Redelings
% October 2026

# NAME

**tree-tool** - Transform one tree or report its properties.

# SYNOPSIS

**tree-tool** _TREE_ [OPTIONS]

# DESCRIPTION

Read one tree, apply the requested transformations, and write the resulting
Newick tree or a selected summary to standard output. The tree retains a
designated root unless an operation removes or replaces it. Other properties,
such as node degree and leaf-to-leaf distance, use the underlying undirected
tree.

Options are applied in a fixed order, regardless of their command-line order:

1. Prune the requested taxa during reading.
2. Remove a single root stem, then repeatedly remove root stems, if requested.
3. Remove degree-two nodes according to **--remove-knuckles**.
4. Resolve polytomies.
5. Scale branch lengths.
6. Strip internal node names, then name unnamed nodes.
7. Find a new root if **--root** is requested.
8. Write the selected output.

# INPUT

_TREE_ is a Newick or NEXUS file, or **-** for standard input. Only its first tree
is used. Supply the filename as the positional argument.
Use branch lengths for scaling, distance summaries, branch joining, and root
finding. Root finding requires a positive total tree length.

# TRANSFORMATION OPTIONS

**--prune** _LIST_
: Remove named taxa. Supply comma-separated names or **@filename** containing
  comma- or newline-separated names. Empty entries are ignored; a backslash
  before an initial literal `@` prevents file interpretation. Unknown names
  are errors. Pruning may suppress intermediate nodes and relocate the root.

**--remove-root-branch**
: If the root has degree one, remove it and its incident branch and move the
  root to the adjacent node. This removes a stem, not a degree-two root.
  The underlying pruning operation can also suppress degree-two nodes created
  by the removal.

**--remove-root-branches**
: Repeat the preceding operation until the root no longer has degree one.

**--remove-knuckles**[**=nonroot**|**=all**]
: Suppress degree-two nodes by joining their two incident branches and adding
  their lengths. With no value, or **=nonroot**, preserve the designated root.
  With **=all**, also suppress a degree-two root; the resulting tree may be
  rooted at a leaf. Without this option, no separate knuckle-removal step runs.
  Labels on suppressed nodes are lost. Use **=all** only when retaining the
  original root position is unnecessary. A later **--root** still selects a
  new root, even when **nonroot** was used.

**--resolve**
: Resolve nodes of degree greater than three by grouping neighboring branches.
  New branches receive zero length where needed. This chooses a resolution
  from the current neighbor order; it does not infer support for that resolution.

**--scale** _FACTOR_
: Multiply all branch lengths by _FACTOR_.

**--strip-internal-names**
: Clear labels on internal nodes.

**--name-all-nodes**
: Assign **node_1**, **node_2**, and so on to unnamed nodes, skipping names
  already present. Existing names are retained.

**--root**
: Search branch positions for a root that approximately minimizes the variance
  of log root-to-leaf distances, with a small offset based on total tree length.
  This is a branch-length-based heuristic, not outgroup rooting or midpoint
  rooting. Use **tree-reroot** for an explicitly selected location.

# OUTPUT OPTIONS

Without an output option, print one Newick tree. Output options describe the
transformed tree. If several are supplied, only the first in the order below
is used:

**--length**
: Print the sum of branch lengths.

**--diameter**
: Print the maximum leaf-to-leaf path length.

**--count-leaves**
: Print the number of leaves.

**--count-nodes**
: Print the number of nodes, including leaves.

**--node-degrees**
: Print a tab-separated degree and label for each node, sorted by decreasing
  degree. Equal-degree order is unspecified; unnamed nodes have an empty label.

**--show-leaves**
: Print leaf names, one per line.

**--show-nodes**
: Print all node names, one per line. Unnamed nodes produce empty lines.

# GENERAL OPTIONS

**-h**, **--help**
: Print help and exit.

**-V**, **--verbose**
: Enable diagnostic output on standard error.

# EXAMPLES

For `tree.nwk` containing `((A:1):2,(B:1,C:1):3)R;`, remove the non-root
knuckle while preserving root R:

```sh
tree-tool tree.nwk --remove-knuckles
```

The result is `(A:3,(B:1,C:1):3)R;`. To suppress the degree-two root as well:

```sh
tree-tool tree.nwk --remove-knuckles=all
```

Scale a tree and inspect its length or node degrees:

```sh
tree-tool tree.nwk --scale 0.5 > scaled.nwk
tree-tool tree.nwk --scale 0.5 --length
tree-tool tree.nwk --node-degrees
```

Prune a taxon from a tree supplied on standard input:

```sh
tree-tool - --prune C < tree.nwk
```

# SEE ALSO

**tree-reroot**(1), **tree-partitions**(1), **trees-pair-distances**(1)

# REPORTING BUGS

BAli-Phy online help: <https://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
