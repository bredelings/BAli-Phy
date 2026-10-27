% draw-tree(1)
% Benjamin Redelings
% October 2026

# NAME

**draw-tree** - Draw NEWICK (and some other) formatted files.

# SYNOPSIS

**draw-tree** _FILE_ [OPTIONS]

# DESCRIPTION

Draw NEWICK (and some other) formatted files. By default, the page fits the complete drawing,
including labels, node bubbles, strokes, and a six-point margin. Fonts and strokes retain their
physical sizes. The initial tree scale uses an 8.5-by-11-inch reference; this does not restrict
final page boundaries.

Supplying only a width or height uniformly resizes the complete drawing and derives the other
dimension from its aspect ratio. Supplying both fits the drawing inside that page and centers it,
adding whitespace rather than distorting the image. Explicit resizing also scales fonts, strokes,
and margins. When both dimensions are supplied, they also guide initial tree scaling and rotation.

Horizontal labels are positioned to avoid branches, node bubbles, and other labels. Crowded labels
move outward, with thin gray dashed lines connecting them to their tips. Placement favors connectors
with fewer branch crossings, even when this moves the text farther away. Crossings may remain, and
these leader lines do not represent evolutionary distances. Angled labels follow terminal branches
and may overlap. Page fitting includes labels and leader lines.

Input may be a Newick tree, a topology-only MC tree with additional partial splits, or
an extended tree with `branch LENGTH` and `node LENGTH` records followed by identifying
partitions. Node lengths describe unresolved length and are accepted only at nodes of
degree at least 4. `mini-branch` records are unsupported. Trees whose branch and node
lengths are all zero can be exported as text but cannot be drawn.

# INPUT OPTIONS:
**-h**, **--help**
: Produce help message

**-W** _arg_, **--width** _arg_
: Output width in inches; otherwise fit the drawing

**-H** _arg_, **--height** _arg_
: Output height in inches; otherwise fit the drawing

_FILE_
: Required positional input tree file

**--output** _arg_ (=pdf)
: Type of output to write: tree, topology, mtree, lengths, dot, ps, pdf, svg.
  Text formats are written to standard output; successful exports return status 0.

**--out** _arg_
: Output filename (without extension)

**--full**
: Consider only full splits by collapsing any partial splits.

**--iterations** _arg_ (=2)
: Number of optimization rounds after graph initialization. The final drawing is written
  once, after both this stage and **--angle_iterations**. If both counts are zero, the
  initialized layout is drawn; initialization itself includes optimization unless
  **--tree-layout-initial** is supplied.

**--font-size** _arg_ (=10)
: Font size in points before explicit output resizing

**--angle_iterations** _arg_ (=0)
: Additional graph optimization rounds with small-angle penalties

**--labels** _arg_ (=horizontal)
: Use `horizontal` for horizontal labels with overlap avoidance, or `angled` for labels aligned
  with terminal branches in equal-angle and equal-daylight layouts. Graph layouts use horizontal labels.

**--collapse**
: Give node lengths evenly to neighboring branches and zero the node lengths.

**--layout** _arg_ (=graph)
: Layout method: graph, equal-angle, equal-daylight. Equal-angle alternates smaller subtrees
  between sides along unequal binary splits to reduce curling in unbalanced trees.
  Equal-daylight starts from this arrangement and balances angular gaps between
  subtrees, including the space occupied by node bubbles. It stops when every accepted rotation
  in a sweep is smaller than 0.001 radians, or after 50 sweeps. Labels are placed afterward.
  Other layout names, including the former `fr` placeholder, are rejected.

**--greedy**
: For equal-daylight layout: redistribute angular gaps equally at each node, preserving
  the cyclic order of its subtrees. Includes branches and node bubbles; skips nodes whose
  summed angular widths fill the circle. Labels are placed afterward.

**--tree-layout-initial**
: Start an energy layout with positions from the equal-angle layout - only for multifurcating trees

**--no-shade**
: For equal-daylight layout: reject rotations that leave subtrees overlapping in angle, including
  their node bubbles.

**--draw-clouds** _arg_
: Draw wandering-ranges in MC trees as clouds.

**--seed** _arg_
: Random seed

**--verbose**
: Output more log messages on stderr.


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

