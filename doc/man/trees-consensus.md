% trees-consensus(1)
% Benjamin Redelings
% October 2026

# NAME

**trees-consensus** - Summarize sampled trees and support for their relationships.

# SYNOPSIS

**trees-consensus** [OPTIONS] _SAMPLED-TREES_ [_SAMPLED-TREES_ ...]

# DESCRIPTION

A sample of evolutionary trees describes uncertainty about how the sampled taxa
(species or other named leaves) are related. **trees-consensus** summarizes that
sample either by counting complete tree topologies or by counting individual
relationships. A topology is the branching arrangement, ignoring branch lengths.

A branch divides the leaves into two groups, called a *split*. The support for a
split is the fraction of retained trees containing it. For trees sampled from a
Bayesian posterior distribution, that fraction estimates its posterior probability
(PP). Support for a split can be high even when no complete topology is common.

By default, write a majority-consensus tree to standard output. It contains splits
present in strictly more than half the retained trees. Unresolved nodes leave
relationships unspecified where the sample does not establish a majority. The
consensus combines relationships and need not be a topology that occurred in the sample.

Other summaries answer different questions:

* **--map-tree** writes the most frequent sampled topology: the maximum a posteriori
  (MAP) topology estimate. Its frequency is support for that complete arrangement,
  not the support of its individual branches.
* **--consensus** requests specified support thresholds. Higher thresholds retain
  fewer relationships. Threshold 1 gives the strict consensus of unanimous splits.
* **--greedy-consensus** considers splits in decreasing support order and adds each
  if it is compatible with those already accepted. This can resolve relationships
  below majority support; inspect the branch labels when interpreting those resolutions.
* **--extended-consensus** describes supported relationships among subsets of taxa.
  This can reveal agreement obscured by taxa whose placement varies across trees.

# INPUT AND SAMPLE SELECTION

Supply one or more Newick or NEXUS tree-sample files. Newick samples contain one
tree per line. Use **-** for standard input. All files must contain the same taxon
set after **--ignore**, although their leaf orders may differ. Trees are interpreted
as unrooted; an artificial degree-two root is suppressed.

Selection is applied separately to each file:

1. Read at most **--until** input trees, if specified.
2. Discard the initial **--skip** trees. The default is **10%**.
3. Keep the first remaining tree and every **--subsample**-th tree thereafter.
4. If more than **--max** trees remain, retain that many at evenly spaced positions.

A percentage skip uses the shortest run's length after **--until** to determine
one common count, rounded down, which is discarded from every run. For example,
runs of 100 and 200 trees with **--skip 10%** each lose 10 trees. This does not
remove 10% independently from each run. Percentages below 100% retain part of every
nonempty run before thinning.

The maximum is per file, not a limit on the pooled total. Thinning to a maximum
includes the first and last selected trees; a maximum of one retains the first.
The retained samples are pooled with equal weight per tree, so a longer retained
run contributes more observations.

# OUTPUT AND INTERPRETATION

Ordinary consensus, greedy, and MAP outputs are Newick trees. Internal branch
labels give split support as fractions between 0 and 1. When branch lengths are
available, each branch receives its mean length over the retained trees containing
that split. Thus a MAP output uses the most frequent topology with estimated branch
lengths, rather than copying one sampled tree's lengths.

The default majority tree is produced only when none of **--consensus**,
**--greedy-consensus**, **--map-tree**, **--extended-consensus**, or
**--extended-consensus-L** is supplied. **--report** alone does not suppress it.
Use named destinations to keep a report and tree output separate.

The optional report includes the sample size, topology and split counts, the most
frequent topologies, a 95% topology credible-set size, and supported splits. The
credible set accumulates topologies in decreasing frequency until their combined
sample frequency reaches 95%; it is not a collection of branch-support intervals.

Report **PP** values are observed fractions, n/N. **LOD** is base-10 log odds with
one added observation on each side: log10((n + 1)/(N - n + 1)). This keeps unanimous
observations finite. It is not a Bayes factor. Partial splits can also have a
**ratio** field: the base-10 logarithm of their odds improvement over the strongest
other reported candidate that implies them.

An extended consensus begins with a Newick tree for full splits, followed by
partial splits such as:

```text
A B | C D [ E ]
```

This expresses the separation of A and B from C and D without specifying E's
placement. The bracketed taxa are excluded from that relationship. Extended output
is therefore richer than a single ordinary Newick tree. **--extended-consensus-L**
adds estimated lengths in the extended format read by **draw-tree**.

# INPUT OPTIONS

**-h**, **--help**
: Print help and exit.

**-s**, **--skip** _COUNT-OR-PERCENT_
: Initial trees to discard from every run. Default: **10%**, using the common
  count described above. Use **0** for an already trimmed sample.

**-u**, **--until** _N_
: Read at most the first _N_ input trees per file, before burn-in and thinning.

**-x**, **--subsample** _N_
: Keep every _N_-th tree after burn-in. Default: 1.

**-m**, **--max** _N_
: Thin each retained run to at most _N_ trees, rather than truncating its end.

**-i**, **--ignore** _LIST_
: Remove named taxa before analysis. Supply comma-separated names or **@filename**
  containing comma- or newline-separated names. A backslash before an initial
  literal `@` prevents file interpretation.

**--seed** _N_
: Initialize the random-number generator with _N_. If omitted, generate a seed.

**-V**, **--verbose**
: Write additional loading and partial-search diagnostics to standard error.

# OUTPUT OPTIONS

**--consensus** _LEVEL[:FILE][,LEVEL[:FILE]...]_
: Write consensus trees at the requested levels, between 0 and 1. A split must
  have support strictly greater than both the level and 0.5; level 1 instead
  retains unanimous splits. For example, support exactly 0.8 is excluded at level
  0.8. Omit **:FILE**, or use **:-**, to write to stdout. Multiple levels are
  processed in increasing order. A list may also be read from **@filename**.

**--map-tree** _FILE_
: Write the most frequent topology with split-support labels and conditional
  mean branch lengths. **-** means stdout.

**--greedy-consensus** _FILE_
: Write the greedy compatible-split summary. **-** means stdout. Equal-support
  alternatives need not give a unique greedy summary.

**--extended-consensus** _LEVEL[:FILE][,LEVEL[:FILE]...]_
: Search for partial splits and write extended summaries at the requested levels.
  Uses the same destination/list syntax as **--consensus**. Levels above 0.5 are
  useful for majority summaries; 1 selects unanimous relationships.

**--extended-consensus-L** _LEVEL[:FILE][,LEVEL[:FILE]...]_
: Write extended summaries with estimated lengths. Uses the same list syntax.

**--report** _FILE_
: Write the topology and split-support report. **-** means stdout. Without this
  option, no report is written.

**--map-trees** _N_
: Include at most _N_ most frequent topologies in the pooled report. Default: 1.
  Does not change the single topology written by **--map-tree**.

**--support-levels** _FILE_
: Write three tab-separated columns: LOD threshold, number of informative full
  splits retained, and support threshold. The LOD calculation uses one pseudocount.

**--extended-support-levels** _FILE_
: Write the corresponding threshold table using the number of informative
  relationships in the compatible extended summary. Use **--sub-partitions**
  to include partial splits in the search. These two table options take literal
  filenames; **-** is not a stdout destination for them.

# PARTIAL-SPLIT SEARCH AND REPORTING

A partial-split search examines subsets of taxa derived from the majority tree,
then uses promising results to explore further subsets. It continues until no new
candidates remain. The controls below concern candidate search and report selection;
they do not replace the thresholds for ordinary or greedy consensus trees.

**--sub-partitions**
: Search for partial splits for the report and support-level tables. Extended
  consensus output options enable this search automatically.

**--min-support** _P_
: Minimum support used when collecting candidate splits for the report and partial
  search. Default: 0.25.

**--rooting** _RATIO_
: Control further exploration when excluding taxa increases support. The search
  compares support before restriction with support after restriction and explores
  promising restrictions below this ratio. Default: 0.9. This controls the subset
  search; it does not choose a root location for an output tree.

**--odds-ratio** _RATIO_
: Suppress redundant partial splits in the report unless their odds improve
  sufficiently over a stronger relationship that implies them. Default: 1.5.
  This report filter uses smoothed odds, adding five observations on each side.

# EXAMPLES

For `sample.trees` containing eight copies of `((A,B),C,D);` and two copies of
`((A,C),B,D);`, use **--skip 0** to analyze all ten. The majority tree retains
AB|CD with support 0.8. A threshold of 0.8 excludes it, producing an unresolved tree:

```sh
trees-consensus sample.trees --skip 0 > majority.tree
trees-consensus sample.trees --skip 0 --consensus 0.8 > conservative.tree
```

Pool retained samples from two runs and write different summaries separately:

```sh
trees-consensus run1.trees run2.trees --map-tree MAP.tree \
  --consensus 0.5:c50.tree,0.8:c80.tree --report support.txt
```

Request a unanimous or greedy summary:

```sh
trees-consensus sample.trees --skip 0 --consensus 1 > strict.tree
trees-consensus sample.trees --skip 0 --greedy-consensus greedy.tree
```

Explore relationships that remain supported when some taxa are excluded:

```sh
trees-consensus run1.trees run2.trees --extended-consensus 0.5:extended.tree \
  --report partial-support.txt
```

# SEE ALSO

**trees-bootstrap**(1), **partitions-supported**(1), **tree-mean-lengths**(1),
**draw-tree**(1), **trees-to-SRQ**(1)

# REPORTING BUGS

BAli-Phy online help: <https://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
