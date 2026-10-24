% tree-mean-lengths(1)
% Benjamin Redelings
% Feb 2018

# NAME

**tree-mean-lengths** - Compute conditional branch and node lengths for query trees.

# SYNOPSIS

**tree-mean-lengths** **--tree** _tree_ [OPTIONS] _file1_ [_file2_ ...]

**tree-mean-lengths** **--trees** _query_ _output_ [**--trees** _query_ _output_ ...]
[OPTIONS] _file1_ [_file2_ ...]

# DESCRIPTION

Compute conditional branch and node lengths for query trees.

Use **--tree** for one query with output on stdout, or repeat **--trees** to
specify query/output filename pairs. Each **--trees** takes exactly two arguments;
remaining positional arguments are sample files. Filenames are literal: commas
and colons are not separators. The two query options are mutually exclusive.

Each query file supplies its first tree, using the existing Newick/NEXUS loader.
All queries must have the same leaf-label set, but need not be nested or compatible.
The sample files are read once, applying **--skip**, **--subsample**, and **--until**
independently to each file. **--ignore** prunes samples; query trees must already
contain the resulting leaf set. All output-mode options apply to every query.

An output filename of **-** means stdout. At most one query may use stdout. Output
filenames must be distinct and must not refer to any query or sample input file.

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

A requested branch or node with no qualifying samples makes that query unavailable.
Without **--safe**, report it, leave its output untouched, write the available queries,
and return failure after attempting all outputs. With **--safe**, write that query's
topology without lengths instead; unavailable estimates then do not cause failure.
A missing node estimate does not prevent branch-only output modes from succeeding.

Invalid queries or destinations fail before processing samples. Sample input or
parsing errors abort estimation before opening outputs, even with **--safe**;
partial estimates are not written. Output opening/writing errors are reported and
other outputs are still attempted, with failure returned at the end. Output writes
are independent, so a write failure may leave its own file incomplete.

Day's interval algorithm matches splits in linear time without constructing
bitsets. Conceptually cutting the matched edges identifies the components whose
lengths contribute to eligible query nodes. Both operations take linear time and
memory per sample per query, excluding tree input. Each query index is constructed
once. Batching shares sample parsing and construction while retaining independent
matching and observation counts for every query; sampled trees are not retained.

# ALLOWED OPTIONS:
**-h**, **--help**
: produce help message

**-T** _arg_, **--tree** _arg_
: One unrooted query tree; write its result to stdout. Cannot be combined with **--trees**.

**--trees** _input_ _output_
: Query and output filenames. Repeat for multiple queries; **-** as output means stdout.

**-s** _arg_ (=0), **--skip** _arg_ (=0)
: number of tree samples to skip

**-u** _arg_, **--until** _arg_
: Read until this number of trees.

**--ignore** _arg_
: Taxa to remove, as a comma-separated list or **@filename**. Files may separate names
  with commas or newlines. Escape an initial literal **@** with a backslash.

**--simple**
: Compute each branch mean from samples containing its exact split, regardless
  of conflicts elsewhere. Missing branch estimates are errors; with **--safe**,
  print that query's topology without lengths instead. Overrides the other output
  modes and does not estimate node lengths.

**-x** _arg_ (=1), **--subsample** _arg_ (=1)
: factor by which to sub-sample

**--var**
: Report population standard deviations conditional on each branch split. Node
  lengths are not estimated. Takes precedence over node-output options.

**--no-node-lengths**
: Report conditional branch means without distributing node lengths.

**--safe**
: For each query with unavailable estimates, report the condition and write its
  topology without lengths. Input, parsing, and output errors still cause failure.

**--show-node-lengths**
: Report branch means, followed by positive node means and their identifying
  partitions in the format consumed by **draw-tree**. Zero node means are omitted.
  Takes precedence over **--no-node-lengths**.

**-V**, **--verbose**
: Report the number of samples and the contribution count for each requested estimate.


# EXAMPLES

Compute separate branch and node means for several consensus trees in one pass:

    tree-mean-lengths --trees c50.tree c50.ltree --trees c80.tree c80.ltree \
      --show-node-lengths --safe run1.trees run2.trees

Write one result to stdout and another to a named file:

    tree-mean-lengths --trees c50.tree - --trees c80.tree c80.ltree \
      --show-node-lengths samples.trees > c50.ltree

# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

