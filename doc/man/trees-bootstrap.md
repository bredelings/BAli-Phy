% trees-bootstrap(1)
% Benjamin Redelings
% October 2026

# NAME

**trees-bootstrap** - Compare estimated relationship support and sampling uncertainty across runs.

# SYNOPSIS

**trees-bootstrap** [OPTIONS] _RUN_ [_RUN_ ...]

**trees-bootstrap** [OPTIONS] _RUN_ [_RUN_ ...] **::** _RUN_ [_RUN_ ...]
[**::** _RUN_ ...]

# DESCRIPTION

Compare how often evolutionary relationships occur in samples of trees. Each
relationship, or *predicate*, is either present or absent in each sampled tree.
Its frequency estimates support; for a Bayesian posterior sample, this estimates
posterior probability (PP).

Successive trees from an MCMC run are usually correlated. Ten consecutive trees
with the same relationship can contain less information than ten independent
observations. **trees-bootstrap** reports support, changes in the event history,
and estimates of autocorrelation and effective sample size where available.
With **--bootstrap**, it also estimates uncertainty in support by resampling
contiguous blocks of that history.

This block bootstrap measures uncertainty from finite tree sampling. It does not
resample alignment columns or calculate the conventional phylogenetic bootstrap
support obtained by repeatedly estimating trees from resampled alignments.

Comparing independent runs helps reveal relationships whose estimated support has
not settled. Similar estimates and narrow intervals are useful evidence of stable
sampling, but runs can agree while all miss an important region of tree space.

# RUNS, GROUPS, AND SAMPLE SELECTION

Each input file is one run, in Newick or NEXUS format. Newick samples contain one
tree per line; **-** reads standard input. Trees are interpreted as unrooted, and
an artificial degree-two root is suppressed. All runs must have the same taxon
set, but their leaf orders may differ.

Adjacent files form one group. Separate groups with **::**:

```sh
trees-bootstrap run1.trees run2.trees :: run3.trees run4.trees
```

Here runs 1 and 2 are summarized individually and pooled as group 1; runs 3 and 4
form group 2. A group with only one run has just that run's summary. Pooling gives
equal weight to retained trees, so longer runs contribute more observations.
Use one group for replicate runs of the same analysis. Separate groups allow
comparisons between analyses or conditions.

Selection proceeds independently per run: **--until**, then **--skip**, then
**--subsample**, then **--max**. The default skip is **10%**. A percentage uses the
shortest run across all groups, after **--until**, to calculate one count rounded
down. That same count is discarded from every run. Thus runs of 100 and 200 trees
with **--skip 10%** both lose 10 trees, before any thinning.

Subsampling retains the first survivor and every Nth tree thereafter. A maximum
thins each run to evenly spaced positions, including its first and last retained
trees; a maximum of one keeps the first. It does not cap the pooled group size.

# PREDICATES

Without **--predicates**, examine splits whose observed support exceeds
**--min-support** in at least one run. A split divides all leaves into two groups,
as an internal branch does. Automatically selected splits are ordered by pooled
occurrence count.

A predicate file selects particular questions. For example:

```text
A B | C D

((A,C),B,D);
```

The first predicate asks whether AB is separated from CD; the second asks for the
relationships in the given Newick tree. Blank lines separate predicates. Multiple
lines within one block form a conjunction: all their relationships must occur.

A partial split such as `A B | C D [ E ]` leaves E's placement unspecified. Every
line in a block must declare the same taxon universe as the samples, including
unused taxa in brackets. Taxa are matched by name. Predicate trees supply topology;
their branch lengths do not affect support. Duplicate predicates are removed.

# READING THE REPORT

The report begins with input filenames and the random seed, pseudocount, and
block size. Each numbered predicate is followed by its per-run and group results.
For a group with several runs, **PP1-1**, **PP1-2**, and so on are individual runs;
**PP1** is their pooled result. **LOD** labels follow the same convention.

For n present observations among N retained trees and pseudocount c:

* **PP** is (n + c)/(N + 2c).
* **LOD** is log10((n + c)/(N - n + c)), the base-10 log odds of presence.
* **1=** and **0=** give actual present and absent counts, without pseudocounts.

Each run contributes its pseudocount to the pooled group, so a group of k runs
uses k times c on each side. LOD is an odds summary, not a Bayes factor.

The bracketed count after the observations is roughly the number of complete
switches out of and back into an event: the number of state changes plus one,
divided by two with integer rounding down. More exclamation marks indicate fewer
changes. Constant or rarely changing histories do not establish precise support
merely because many trees were recorded.

**ACT** is the estimated autocorrelation time in retained-tree units. **Ne** is
the number of retained trees divided by ACT, an effective sample-size estimate for
that predicate. These fields are omitted when there are too few changes to estimate
them. Pooled histories concatenate runs, so their transition and autocorrelation
summaries can include effects of the joins.

With **--bootstrap**, parentheses following PP and LOD show central bootstrap
intervals with probability **--confidence**. More than ten bootstrap replicates
are needed for intervals to be printed. **sigma**, when shown for sufficiently
changing histories, is the bootstrap standard deviation of the support estimate.

For groups with at least two runs, **ASDSF** is the average standard deviation of
split frequencies between runs, considering splits above **--min-support** in at
least one member. **MSDSF** is the maximum standard deviation over the available
splits. These use observed frequencies without pseudocount smoothing. ASDSF is
reported as `NA` when no splits pass the group's cutoff; MSDSF is `NA` when the
split collection is empty. **min Ne**
identifies the smallest available effective sample-size estimate among the group's
analyzed predicates. No single cutoff for these diagnostics establishes convergence.

# INPUT OPTIONS

**-h**, **--help**
: Print help and exit.

**--files** _FILE_ ...
: Named form of the positional input list, including any **::** separators.

**-s**, **--skip** _COUNT-OR-PERCENT_
: Discard this initial count per run, or use a common percentage-derived count.
  Default: **10%**. Use **0** for already trimmed runs.

**-u**, **--until** _N_
: Read at most _N_ input trees per run, before burn-in and thinning.

**-x**, **--subsample** _N_
: Keep every _N_-th tree after skipping. Default: 1.

**-m**, **--max** _N_
: Thin each selected run to at most _N_ trees.

**--predicates** _FILE_
: Read predicates from _FILE_ instead of automatically selecting splits.

**--min-support** _P_
: Observed support threshold for automatic split selection and the ASDSF summary.
  Default: 0.1. Does not filter a supplied predicate list.

**-V**, **--verbose**
: Write additional diagnostics to standard error.

# BOOTSTRAP AND REPORTING OPTIONS

**--bootstrap**
: Resample contiguous blocks to estimate sampling uncertainty. Without this flag,
  report support and available mixing diagnostics without bootstrap intervals.

**--samples** _N_
: Number of bootstrap replicates. Default: 10000. Used with **--bootstrap**.

**--blocksize** _N_
: Number of consecutive observations per resampled block. Default: the shortest
  retained run's length divided by 50, rounded down, plus one. Block size 1 treats
  observations independently; larger blocks retain more local dependence. Blocks
  are sampled from the binary history augmented with pseudocount observations.
  A requested block larger than that history is capped at its length.

**--pseudocount** _N_
: Add _N_ present and _N_ absent observations per run when estimating support and
  resampling. Default: 1. Pooling sums the contributions of the group's runs.

**--seed** _N_
: Use a specified random seed for reproducible resampling. If omitted, generate
  a seed. The seed used is printed in the report.

**--confidence** _P_
: Probability contained in central bootstrap intervals. Default: 0.95. Also the
  probability threshold used for **--separation**.

**--separation** _LOD-DIFFERENCE_
: Restrict predicate reports to those for which at least one pair of group
  summaries is separated by more than this amount in bootstrap log10 odds, with
  probability at least **--confidence**. Default: 0, which does not filter reports.
  A positive value with multiple groups requires **--bootstrap**. With one group,
  no separation filtering is performed. Group diagnostics are still reported.

**--LOD-table** _FILE_
: Write a tab-separated table of LOD values, one row per analyzed predicate, for
  the first group only. Columns are its individual runs followed by the pool when
  there is more than one run. There is no header. The table includes predicates
  suppressed from the text report by **--separation**. _FILE_ is a literal filename.

# EXAMPLES

Compare replicate runs with a reproducible block bootstrap:

```sh
trees-bootstrap run1.trees run2.trees --bootstrap --seed 1 > support.txt
```

Ask about the split AB|CD explicitly:

```sh
printf 'A B | C D\n' > predicates.txt
trees-bootstrap run1.trees run2.trees --predicates predicates.txt --skip 0
```

For ten retained trees, eight supporting that split, the observed frequency is
0.8. With the default pseudocount, the report gives PP = 9/12 = 0.750 and
LOD = log10(9/3) = 0.477. The actual counts remain **1=8 0=2**.

Compare two groups and report predicates with a bootstrap-supported difference
of more than one unit of log10 odds:

```sh
trees-bootstrap run1.trees run2.trees :: run3.trees run4.trees \
  --bootstrap --separation 1 --seed 1
```

Export the first group's LOD table:

```sh
trees-bootstrap run1.trees run2.trees --LOD-table support-lod.tsv > support.txt
```

# SEE ALSO

**trees-consensus**(1), **partitions-supported**(1), **trees-to-SRQ**(1),
**trees-distances**(1)

# REPORTING BUGS

BAli-Phy online help: <https://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
