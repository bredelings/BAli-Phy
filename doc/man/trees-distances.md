% trees-distances(1)
% Benjamin Redelings
% October 2026

# NAME

**trees-distances** - Compare sampled tree topologies, branch lengths, and distance histories.

# SYNOPSIS

**trees-distances** _ANALYSIS_ \[OPTIONS\] _FILE_ [_FILE_ ...]

# DESCRIPTION

Measure how different sampled evolutionary trees are, summarize differences within
or between runs, or examine how those differences change along a sampling history.
These comparisons can help identify persistent differences between runs and slow
movement through tree space.

Specify the analysis before the input files, for example
**trees-distances matrix sample.trees**.

The default distance compares branching arrangements, ignoring branch lengths.
Other metrics compare branch lengths or match similar splits. A *split* is the
bipartition of the named leaves obtained by cutting a branch. Distances match taxa
by name, so rotating a drawing or reordering a tree's Newick branches does not
change its topology.

Most analyses compare whole trees. The **leaf-dist-matrix** and **node-dist-cvars**
analyses instead measure paths within each tree. For mean leaf-to-leaf distances
across a sample, use **trees-pair-distances**.

# INPUT AND SAMPLE SELECTION

Supply Newick or NEXUS tree samples; Newick samples have one tree per line. **-**
reads standard input. All compared trees must have the same taxon set, but input
leaf orders may differ. Trees are interpreted as unrooted; an artificial degree-two
root is suppressed. Supply nonnegative branch lengths for branch-length metrics
and path-length analyses.

Select trees separately per input file: read at most **--until** input trees,
discard **--skip**, keep the first survivor and every **--subsample**-th tree, then
thin to **--max** evenly spaced retained positions if necessary. The maximum
includes both endpoints; a maximum of one keeps the first. It is a per-file cap,
not a cap on the combined sample. Skip is an integer count, default 0.

The **convergence** and **converged** analyses are exceptions: selection options
apply only to the first file. The second file is the full reference sample.

Matrix-based analyses store pairwise distances, so memory and computation grow
approximately with the square of the number of retained trees. Subsampling or a
per-file maximum can make large comparisons more manageable.

# METRICS

**topology**, **RF**
: Count internal splits present in one tree but absent from the other: the
  unnormalized Robinson-Foulds distance. Branch lengths are ignored. Replacing
  one split by a different split contributes two, whereas losing a split in an
  unresolved tree contributes one. This is the default metric.

**branch**, **branches**
: Sum absolute differences of corresponding branch lengths, including pendant
  branches leading to leaves. A split absent from one tree has length zero there.
  The result has the same units as the input branch lengths.

**internal-branch**
: Use the same absolute-difference calculation for internal branches only.

**matching**
: Find a minimum-cost matching of internal splits. The cost of matching two splits
  is the smallest number of taxa whose side membership must change, allowing the
  two sides to be reversed. An unmatched split costs the number of taxa on its
  smaller side. Branch lengths are ignored.

# ANALYSES

**matrix** — one or more files
: Write a tab-separated square matrix to stdout, without row names or a header.
  Rows and columns follow the retained tree order, concatenating files in command
  order. Distances are symmetric with zero diagonals unless **--jitter** is used.

**autocorrelation** — one file
: Write two columns: lag in retained trees and mean distance between pairs at
  that lag. Lag zero compares each tree with itself. This is a distance-versus-lag
  curve, not a normalized autocorrelation coefficient. A rise with lag can show
  that nearby samples are more alike than widely separated samples. Use
  **--max-lag** to control its extent.

**diameter** — one file
: Summarize the distribution of distances between distinct pairs of retained
  trees, and each tree's mean distance to the others. Despite the name, this
  does not print only the largest distance; **--minmax** includes that value.
  Requires at least two retained trees.

**compare** — two files
: Report within-file and between-file distance summaries, followed by empirical
  comparisons of those distributions. This helps assess whether differences
  between runs resemble the variation within each run.

**closest** — two files
: Report the smallest between-file distance and the corresponding pair of trees.
  The reported tree indices are one-based positions in the retained samples.

**convergence** — query file, reference file
: Write one value per retained query tree: its mean distance to all reference
  trees. The reference is not thinned or trimmed by selection options. A stable
  trace can be informative, but depends on the reference sample and metric.

**converged** — query file, reference file
: Apply a boundary-crossing diagnostic to that mean-distance trace. Reference
  trees' mean distances to other reference trees, excluding self-comparisons,
  define lower and upper quantiles. Beginning after the first query tree, look
  for four strict crossings alternating below the lower and above the upper
  boundary. Print **Stabilized** if all four occur; otherwise report how many
  occurred. The reference requires at least two trees. This heuristic does not
  establish convergence of the full posterior distribution.

  **First hit** uses a zero-based retained-query index. **Stabilized** reports the
  number of retained query trees processed through the final crossing, one more
  than that crossing's zero-based index. Neither label is an original MCMC
  iteration number. The final closest-distance line is the smallest mean distance
  encountered before the diagnostic stops.

**leaf-dist-matrix** — one file
: For each retained tree, print a tab-separated leaf-name header followed by its
  leaf-to-leaf path-length matrix, with no row labels. **--topology-only** counts
  path edges instead. **--metric** does not select these within-tree distances.

**node-dist-cvars** — one file
: Print a comma-separated header and one row per retained tree. By default,
  columns give path lengths for every unordered pair of nodes, labelled
  **D1-0**, **D2-0**, and so on. Internal node numbers need not correspond across
  different topologies; use a common topology for comparisons of those columns.
  **--leaves-only** instead reports pendant branch lengths, labelled **L0**, **L1**,
  and so on, in leaf order. **--topology-only** assigns unit lengths to branches.
  **--metric** does not select these quantities.

# READING DISTANCE SUMMARIES

For **diameter** and **compare**, the default is a median followed by a central
empirical interval containing **--CI** of the distance observations. This describes
the spread of distances; it is not a confidence interval for their mean.
**--mean**, **--median**, and **--minmax** select summaries and may be combined.

The report names distinguish different distributions:

* **D11** and **D22**: distances between distinct tree pairs within files 1 and 2.
* **D1(1)** and **D2(2)**: each tree's mean distance to the other trees in its file.
* **D12**: all distances between a tree in file 1 and a tree in file 2.
* **D1(2)** and **D2(1)**: each tree's mean distance to the opposite file.

A line such as **P(D12 > D11)** compares the empirical distributions, counting
half of ties on each side. It is not a calibrated hypothesis-test p-value. If the
two input files are identical, between-file comparisons include self-matching
trees, whereas within-file comparisons exclude self-pairs.

# OPTIONS

**-h**, **--help**
: Print help and exit.

**--metric** _NAME_
: Whole-tree distance metric from the list above. Default: **topology**.

**-s**, **--skip** _N_
: Discard _N_ initial trees per selected file. Default: 0; percentages are not accepted.

**-u**, **--until** _N_
: Read at most _N_ input trees per selected file before skipping and thinning.

**-x**, **--subsample** _N_
: Keep every _N_-th tree after skipping. Default: 1.

**-m**, **--max** _N_
: Thin each selected file to at most _N_ trees.

**--remove-duplicates**
: In **matrix**, retain the first representative of trees at zero distance under
  the chosen metric. This can remove different branch-length observations when
  using the topology metric. Output has no index map back to the omitted trees.

**--max-lag** _N_
: In **autocorrelation**, include lags zero through _N_, capped at half the retained
  sample size. By default the number of lags is the smaller of floor(size/10) + 1
  and floor(size/2). An explicit zero requests only lag zero.

**--CI** _P_
: Probability covered by central empirical intervals in **diameter** and **compare**.
  Default: 0.95.

**--mean**
: Show the mean and standard deviation in distance summaries.

**--median**
: Show the median and central empirical interval. Used by default when none of
  the three summary flags is supplied.

**--minmax**
: Show the minimum and maximum in distance summaries.

**--converged** _P_
: Lower reference quantile for **converged**; the upper quantile is 1 - _P_. Default:
  0.05. Values must lie strictly between 0 and 1; values above 0.5 are replaced by
  1 - _P_. Smaller values require more extreme crossings.

**--leaves-only**
: In **node-dist-cvars**, report pendant lengths instead of node-pair distances.

**--topology-only**
: In **node-dist-cvars** and **leaf-dist-matrix**, use unit branch lengths.

**--jitter** _SD_
: In **matrix**, add independent Gaussian noise with this standard deviation to
  every entry, after duplicate removal. This can break symmetry and zero diagonals
  and produce negative entries; the perturbed output is no longer a distance matrix
  in the usual mathematical sense.

**-V**, **--verbose**
: Write additional loading diagnostics to standard error.

# EXAMPLES

For `pair.trees` containing:

```text
((A:1,B:1):1,C:1,D:1);
((A:1,C:1):1,B:1,D:1);
```

The off-diagonal topology distance is 2, because AB|CD is replaced by AC|BD:

```sh
trees-distances matrix pair.trees
```

Compare branch-length variation between two runs, retaining at most 100 trees per file:

```sh
trees-distances compare run1.trees run2.trees --metric branch --max 100 --mean --median
```

Examine distance versus lag, or a query run's distance from a reference sample:

```sh
trees-distances autocorrelation run1.trees --max-lag 20 > lag-distances.txt
trees-distances convergence run1.trees reference.trees --skip 10 > reference-distances.txt
```

Measure leaf-to-leaf path distances within each tree:

```sh
trees-distances leaf-dist-matrix pair.trees
```

# SEE ALSO

**trees-pair-distances**(1), **trees-bootstrap**(1), **trees-to-SRQ**(1),
**trees-consensus**(1), **tree-tool**(1)

# REPORTING BUGS

BAli-Phy online help: <https://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
