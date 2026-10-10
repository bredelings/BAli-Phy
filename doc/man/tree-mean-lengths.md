% tree-mean-lengths(1)
% Benjamin Redelings
% Feb 2018

# NAME

**tree-mean-lengths** - Compute the mean lengths for branches in the given topology.

# SYNOPSIS

**tree-mean-lengths** **--tree** _tree_ [OPTIONS] _file1_ [_file2_ ...]

# DESCRIPTION

Compute the mean lengths for branches in the given topology.

Trees are interpreted as unrooted. An artificial degree-two Newick root is
suppressed in both the query and sampled trees; its two branches form one edge.
Internal labels do not select rooted comparisons.

Each branch mean uses only samples containing that branch's exact split. For a
multifurcating query node, a sample qualifies only if it contains every split
incident to that node. Its node-length observation is the sum of lengths of the
sample branches resolving that multifurcation. These branches lie between the
matched incident branches. A qualifying sample with no resolving branches, or
with resolving branches of total length zero, contributes zero.

Each branch and node has its own contributing-sample count. Conflicts elsewhere
in a sample do not exclude it. Samples may themselves contain multifurcations.
By default, each node mean is divided equally among its incident query branches
and added to their branch means. Use **--show-node-lengths** to keep the two kinds
of estimates separate, or **--no-node-lengths** for branch means alone.

A requested branch or node with no qualifying samples is an error. **--safe**
prints a diagnostic and the query topology without lengths, returning success.
A missing node estimate does not prevent branch-only output modes from succeeding.

Day's interval algorithm matches splits in linear time without constructing
bitsets. Conceptually cutting the matched edges identifies the components whose
lengths contribute to eligible query nodes. Both operations take linear time and
memory per sample, excluding tree input. The query index is constructed once.

# ALLOWED OPTIONS:
**-h**, **--help**
: produce help message

**-T** _arg_, **--tree** _arg_
: unrooted query tree

**--files** _arg_
: tree samples to examine

**-s** _arg_ (=0), **--skip** _arg_ (=0)
: number of tree samples to skip

**-u** _arg_, **--until** _arg_
: Read until this number of trees.

**--ignore** _arg_
: Comma-separated taxa to remove

**--simple**
: Compute each branch mean from samples containing its exact split, regardless
  of conflicts elsewhere. Missing branch estimates are errors; with **--safe**,
  print the query topology without lengths instead. Overrides the other output
  modes and does not estimate node lengths.

**-x** _arg_ (=1), **--subsample** _arg_ (=1)
: factor by which to sub-sample

**--var**
: Report population standard deviations conditional on each branch split. Node
  lengths are not estimated. Takes precedence over node-output options.

**--no-node-lengths**
: Report conditional branch means without distributing node lengths.

**--safe**
: On an input or unavailable-estimate error during sample processing, report the
  error and print the query topology without lengths, returning success.

**--show-node-lengths**
: Report branch means, followed by positive node means and their identifying
  partitions in the format consumed by **draw-tree**. Zero node means are omitted.
  Takes precedence over **--no-node-lengths**.

**-V**, **--verbose**
: Report the number of samples and the contribution count for each requested estimate.


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

