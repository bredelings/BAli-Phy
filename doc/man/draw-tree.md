% draw-tree(1)
% Benjamin Redelings
% Feb 2018

# NAME

**draw-tree** - Draw NEWICK (and some other) formatted files.

# SYNOPSIS

**draw-tree** [OPTIONS] _tree file_

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
move outward to nearby available space, with thin gray dashed lines connecting them to their tips. These leader
lines do not represent evolutionary distances and may cross other drawing elements. Angled labels
follow terminal branches and may overlap. Page fitting includes labels and leader lines.

# INPUT OPTIONS:
**-h**, **--help**
: Produce help message

**-W** _arg_, **--width** _arg_
: Output width in inches; otherwise fit the drawing

**-H** _arg_, **--height** _arg_
: Output height in inches; otherwise fit the drawing

**--file** _arg_
: predicates to examine

**--output** _arg_ (=pdf)
: Type of output to write: tree, topology, mtree, lengths, dot, ps, pdf, svg

**--out** _arg_
: Output filename (without extension)

**--full**
: Consider only full splits by collapsing any partial splits.

**--iterations** _arg_ (=2)
: Number of iterations for the graph layout algorithm

**--font-size** _arg_ (=10)
: Font size in points before explicit output resizing

**--angle_iterations** _arg_ (=0)
: Number of iterations for layout algorithm with small-angle penalties

**--labels** _arg_ (=horizontal)
: Use `horizontal` for horizontal labels with overlap avoidance, or `angled` for labels aligned
  with terminal branches in equal-angle and equal-daylight layouts. Graph layouts use horizontal labels.

**--collapse**
: Give node lengths evenly to neighboring branches and zero the node lengths.

**--layout** _arg_ (=graph)
: Layout method: graph, equal-angle, equal-daylight. Equal-daylight balances angular gaps between
  subtrees, including the space occupied by node bubbles. It stops when every accepted rotation
  in a sweep is smaller than 0.001 radians, or after 50 sweeps. Labels are placed afterward.
  The former `fr` placeholder is not implemented.

**--greedy**
: For equal-daylight layout: take as much daylight as possible?

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

