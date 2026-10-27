% draw-graph(1)
% Benjamin Redelings
% October 2026

# NAME

**draw-graph** - Write a multiconnected tree in Graphviz DOT format.

# SYNOPSIS

**draw-graph** _file_ \[OPTIONS\]

# DESCRIPTION

Convert a tree or collection of taxon splits into a graph description for Graphviz to
render. A split separates taxa into two groups. A partial split leaves some taxa's
placement unspecified. A multiconnected tree represents these relationships while allowing
some branches to attach in more than one place.

The command writes Graphviz DOT text to standard output. Use a Graphviz layout program,
such as **dot**, to turn it into an image. The drawing represents relationships and
placement flexibility; it is not scaled by branch lengths.

# INPUT

The required positional _file_ contains a Newick tree, partition lines, or both. Put each
partition on its own line. A Newick tree must occupy one line beginning with `(`; its
branches are converted to splits. NEXUS input is not supported here.

Partition lines list names on either side of `|`, with taxa whose placement is unspecified
in brackets. For example:

```
a b | c d [ e ]
```

This separates `a` and `b` from `c` and `d` without assigning `e` to either side. A full
split includes every taxon on one of the two sides and needs no bracketed group. Use the
same taxon set throughout a collection, including the bracketed names, and separate names
and punctuation with spaces.

Blank lines separate collections. Only the first nonempty collection is drawn. Conflicting
partitions may be removed to obtain a collection representable as a multiconnected tree;
the output need not retain every supplied partition.

# OPTIONS

**-h**, **`--help`**
: Print help and exit.

# OUTPUT

The DOT graph labels tips with taxon names and displays internal nodes as small circles.
Solid edges represent splits; edges for partial splits are green. Dashed connections
represent flexibility in branch attachment. Their arrows are part of that representation,
not inferred directions of evolution.

Branch lengths and support values are not displayed. Errors go to standard error.

# EXAMPLE

For a file `splits.txt` containing the partial split above, write DOT and render an SVG:

```bash
draw-graph splits.txt > graph.dot
dot -Tsvg graph.dot -o graph.svg
```

Graphviz must be installed to run the second command. The same commands also work with a
file containing a single Newick tree.

# SEE ALSO

**draw-tree**(1), **tree-partitions**(1), **dot**(1)

# REPORTING BUGS

BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
