% draw-tree(1)
% Benjamin Redelings
% October 2026

# NAME

**draw-tree** - Draw phylogenetic trees and uncertain attachments.

# SYNOPSIS

**draw-tree** _FILE_ [OPTIONS]

# DESCRIPTION

**draw-tree** draws ordinary phylogenetic trees and multiconnected (MC) trees, which can
leave some taxa's attachments unspecified. It uses supplied relationships and lengths;
it does not estimate them from sequence data or sampled trees. Output may be a PDF,
PostScript, or SVG picture, or a textual representation of the tree.

Ordinary branches represent splits of taxa. In MC drawings, black branches represent full
splits and green branches represent partial splits. Dashed attachment edges indicate where
an incompletely placed group can attach. Clouds provide an alternative way to show these
possible attachment ranges. Their colors help distinguish overlapping ranges; they do not
encode probabilities.

Node bubbles represent branch length within unresolved regions. A node length is the total
length assigned to branches resolving that region, not a node age or a confidence interval.
Bubble size is derived from this length and the number of incident branches.

# INPUT

_FILE_ is a required positional filename. It may contain an ordinary Newick tree, with or
without branch lengths. Missing Newick branch lengths are assigned length 1.

An MC-tree description can contain a Newick topology followed by partial splits. A full
split places every taxon on one side or the other; a partial split leaves some taxa's sides
unspecified. For example:

    (A,B,C,D,E);
    A B | C D [ E ]

The extra split separates A and B from C and D, leaving E's side unspecified. Bare partial
splits in this topology-only format receive length 1. Redundant partitions are removed,
and conflicting partial partitions may be discarded to obtain an MC-tree representation.
**--verbose** reports partition-selection diagnostics.

For estimated lengths, the extended format uses `branch LENGTH` followed by a partition,
and `node LENGTH` followed by a directed partition identifying a node. These records may
follow a Newick tree with branch lengths, or supply the lengths without an initial Newick
tree. In a node record, the partition identifies the node at the end of the branch directed
from its left group toward its right group; the number is a node length, not the length of
that identifying branch. Node-length records are accepted only at nodes with at least four
incident branches.

**tree-mean-lengths --show-node-lengths** produces ordinary trees with separate node lengths.
**mctree-mean-lengths** also produces lengths for partial splits. Trees whose branch and
node lengths are all zero can be exported as text but cannot be drawn.

# LAYOUT AND LABELS

The default **graph** layout supports both ordinary and MC trees. It optimizes branch-length
constraints while separating nodes; displayed branch lengths are approximate.

**equal-angle** and **equal-daylight** support ordinary trees, including unresolved nodes
with bubbles. Use **--full** first to draw an MC tree with either layout. Equal-angle
allocates angular space according to the number of descendant tips. Equal-daylight starts
there and rotates subtrees to balance the empty angular gaps between them. These tree
layouts preserve branch lengths up to overall scaling, with extra space for endpoint
bubbles. They do not require the relationships to be fully resolved.

Horizontal labels are positioned to avoid branches, bubbles, and other labels. Crowded
labels move outward, with thin gray dashed lines connecting them to their tips. These label
connectors do not represent evolutionary distances or uncertain attachments. Placement
favors fewer branch crossings, but crossings may remain. Angled labels follow terminal
branches and may overlap. Labels are placed after the tree layout.

# OUTPUT

Select the format with **--output** (default: `pdf`). Tree transformations such as **--full**
and **--collapse** are applied before export.

`pdf`, `ps`, `svg`
: PDF, PostScript, or SVG drawing, written to a file.

`tree`
: Ordinary full-split tree in Newick with branch lengths. Omits partial splits and node
  lengths. Written to standard output.

`topology`
: Ordinary full-split Newick topology, without lengths or partial splits. Written to
  standard output.

`mtree`
: Newick topology followed by partial splits, without branch or node lengths. Written to
  standard output.

`lengths`
: Full and partial `branch LENGTH` records and positive `node LENGTH` records. Written to
  standard output.

`dot`
: Graphviz graph description with length and node-size attributes for drawing. Written to
  standard output.

By default, pictures are written in the current directory. The basename is the input
filename with its final extension removed, followed by `-mctree` for graph layout or `-tree`
for either tree layout. The output-format extension is then appended. **--out** replaces
that basename and may include a directory; the extension is still appended. For example,
`--out figures/consensus --output svg` writes `figures/consensus.svg`. **--out** does not
redirect text exports; use shell redirection for those. Successful exports return status 0.

# OPTIONS

## Output and sizing

**--output** _FORMAT_ (default: pdf)
: Select one of the formats listed above.

**--out** _BASENAME_
: Set the picture filename without its output-format extension.

**-W** _INCHES_, **--width** _INCHES_
: Set output width. Must be finite and positive.

**-H** _INCHES_, **--height** _INCHES_
: Set output height. Must be finite and positive.

**--font-size** _POINTS_ (default: 10)
: Set font size before explicit output resizing.

Without explicit dimensions, the page fits the complete drawing, including labels,
connectors, bubbles, strokes, and a six-point margin. Fonts and strokes retain their
physical sizes. An 8.5-by-11-inch reference guides initial tree scaling but does not
restrict final page boundaries.

Supplying only a width or height uniformly resizes the complete drawing and derives the
other dimension. Supplying both fits and centers the drawing inside that page, adding
whitespace without stretching it. Explicit resizing also scales fonts, strokes, and
margins. When both dimensions are supplied, they also guide initial scaling and rotation.

## Layout

**--layout** _METHOD_ (default: graph)
: Select `graph`, `equal-angle`, or `equal-daylight`, as described above.

**--labels** _STYLE_ (default: horizontal)
: Select `horizontal` or `angled` labels for equal-angle and equal-daylight layouts.
  Graph layout uses horizontal labels.

**--draw-clouds** _MODE_
: In graph layout, draw possible attachment ranges as clouds. Use `only` to hide dashed
  attachment edges and clip attached branches at the clouds. Other values draw clouds
  alongside the attachment edges. Omitting the option draws no clouds. A value is required.

**--iterations** _N_ (default: 2)
: Number of graph optimization rounds after initialization.

**--angle_iterations** _N_ (default: 0)
: Additional graph optimization rounds with penalties for small angles between branches.

The final graph drawing is written once after both optimization stages. If both counts are
zero, the initialized layout is drawn. Initialization itself includes optimization unless
**--tree-layout-initial** is supplied. These counts do not control equal-daylight iterations.

**--tree-layout-initial**
: Initialize graph layout from equal-angle positions instead of the usual graph
  initialization. Applicable only when the graph is an ordinary tree with no partial splits.

**--greedy**
: For equal-daylight layout, redistribute available angular gaps equally at each node,
  preserving the cyclic order of its subtrees. Includes the angular space occupied by
  branches and bubbles.

**--no-shade**
: For equal-daylight layout, reject rotations that leave subtrees overlapping in angle,
  including their bubbles.

## Tree transformations

**--full**
: Collapse partial splits into the ordinary full-split tree, accumulating their lengths
  at the resulting unresolved nodes.

**--collapse**
: Distribute each node length evenly among its incident branches, then set node lengths
  to zero. With **--full**, this runs after partial splits have been collapsed.

## General

**--seed** _N_
: Set the random seed used during partition selection and graph initialization.

**--verbose**
: Write additional diagnostics to standard error.

**-h**, **--help**
: Show command-line help.

# EXAMPLES

Draw an ordinary tree as `consensus-tree.pdf`:

    draw-tree consensus.tree --layout equal-daylight

Draw an MC tree with clouds showing possible attachments:

    draw-tree consensus.mlengths --draw-clouds=only --output svg

Write a six-inch-wide SVG with a chosen basename:

    draw-tree consensus.tree --layout equal-daylight --width 6 --output svg --out figure

Export the ordinary topology without lengths:

    draw-tree consensus.tree --output topology > topology.tree

# SEE ALSO

**tree-mean-lengths**(1), **mctree-mean-lengths**(1), **trees-consensus**(1), **draw-graph**(1)

# REPORTING BUGS

BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
