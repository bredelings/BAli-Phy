% draw-tree(1)
% Benjamin Redelings
% Feb 2018

# NAME

**draw-tree** - Draw NEWICK (and some other) formatted files.

# SYNOPSIS

**draw-tree** [OPTIONS] _tree file_

# DESCRIPTION

Draw NEWICK (and some other) formatted files.

# INPUT OPTIONS:
**-h**, **--help**
: Produce help message

**-W** _arg_ (=8.5), **--width** _arg_ (=8.5)
: Page width in inches

**-H** _arg_ (=11), **--height** _arg_ (=11)
: Page height in inches

**--file** _arg_
: predicates to examine

**--output** _arg_ (=pdf)
: Type of output to write: tree, topology, mtree, lengths, dot, ps, pdf, svg

**--out** _arg_
: Output filename (without extension)

**--full**
: Consider only full splits by collapsing any partial splits.

**--iterations** _arg_ (=2)
: Number of iterations for layout algorithm

**--font-size** _arg_ (=10)
: Font size for taxon names

**--angle_iterations** _arg_ (=0)
: Number of iterations for layout algorithm with small-angle penalties

**--labels** _arg_ (=horizontal)
: Are the names horizontal or angled?

**--collapse**
: Give node lengths evenly to neighboring branches and zero the node lengths.

**--layout** _arg_ (=graph)
: Layout method: graph, equal-angle, equal-daylight, etc.

**--greedy**
: For equal-daylight layout: take as much daylight as possible?

**--tree-layout-initial**
: Start an energy layout with positions from the equal-angle layout - only for multifurcating trees

**--no-shade**
: For equal-daylight layout: reject rotations that shade other groups?

**--draw-clouds** _arg_
: Draw wandering-ranges in MC trees as clouds.

**--seed** _arg_
: Random seed

**--verbose**
: Output more log messages on stderr.


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

