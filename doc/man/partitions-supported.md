% partitions-supported(1)
% Benjamin Redelings
% October 2026

# NAME

**partitions-supported** - Select supplied relationships by their support in a tree sample.

# SYNOPSIS

**partitions-supported** _PREDICATES_ [_SAMPLED-TREES_] \[OPTIONS\]

# DESCRIPTION

Select relationships that occur within a specified fraction of sampled evolutionary
trees. For example, extract strongly supported relationships, relationships with
intermediate support, or those that fail a chosen support condition. The selected
predicates can be used as input to other tools, such as **trees-to-SRQ**.

A *split* divides a tree's leaves into two groups separated by a branch. A
*predicate* asks whether a split, or a collection of splits, is present. Its support
is the fraction of retained trees satisfying all the specified relationships.
For a Bayesian posterior sample, this estimates their joint posterior probability.

This tool evaluates the predicates supplied to it; it does not discover splits or
build a consensus tree. Use **trees-consensus** to summarize relationships across
the sample, or **trees-bootstrap** to examine support and sampling uncertainty.

# PREDICATE FILE

Blank lines separate predicates. A split can be written as:

```text
A B | C D
```

This asks whether a branch separates A and B from C and D. The two sides are
interchangeable. A partial split leaves some taxa's positions unspecified:

```text
A B | C D [ E ]
```

Here the split is evaluated after disregarding E. Include excluded taxa in the
bracketed list so the declared taxon set still matches the samples.

A Newick tree describes the conjunction of its splits. An unresolved tree leaves
some relationships unspecified. Multiple split or tree lines within one block
also form a conjunction, not separate predicates. For example, this single block
asks for both relationships in a six-taxon sample:

```text
A B | C D E F
A B C D | E F
```

Use the same declared taxon universe on every line and in the sampled trees.
Taxa are matched by name; their input orders may differ. Predicate branch lengths
are ignored. Duplicate predicates are removed before filtering.

# TREE INPUT AND SELECTION

The sample is a Newick or NEXUS file; Newick samples contain one tree per line.
Omit _SAMPLED-TREES_, or supply **-**, to read it from standard input. Trees are
interpreted as unrooted; an artificial degree-two root is suppressed.

Read at most **--until** input trees, discard **--skip** initial trees, then keep
the first survivor and every **--subsample**-th tree thereafter. Finally,
**--max** thins to evenly spaced retained positions if necessary. It includes
both endpoints; a maximum of one keeps the first. This is not first-N truncation.

The skip is an integer count, default 0; this tool does not accept percentage
burn-in. Support uses the final retained sample and has no pseudocount smoothing.

# SELECTION AND OUTPUT

Bounds are strict. With **--above 0.8**, a predicate supported by exactly eight of
ten trees is excluded. **--above 0.5 --below 0.9** retains support strictly between
those values. An omitted bound imposes no restriction; with neither bound, all
supplied predicates are selected.

**--not** complements the whole condition. For example, with both bounds it selects
predicates at or outside the endpoints. With no bounds, **--not** selects none.

Write selected predicates to standard output as split lines, with a blank line
between predicates. A Newick predicate is expanded into its splits rather than
copied as a tree. No support numbers or table header are added. Output can be
reused as a predicate file. File-loading messages go to standard error.

# OPTIONS

**-h**, **--help**
: Print help and exit.

**--predicates** _FILE_
: Named form of the required positional predicate file.

**--file** _FILE_
: Named form of the tree-sample file. Default: **-**, standard input.

**--skip** _N_
: Discard _N_ initial trees. Default: 0.

**--until** _N_
: Read at most the first _N_ input trees before skipping and thinning.

**--subsample** _N_
: Keep every _N_-th tree after skipping. Default: 1.

**--max** _N_
: Thin the selected sample to at most _N_ trees.

**-a**, **--above** _P_
: Require support strictly greater than _P_, a fraction between 0 and 1.

**-b**, **--below** _P_
: Require support strictly less than _P_.

**-n**, **--not**
: Select the complement of the combined support condition.

**--verbose**
: Write additional loading diagnostics to standard error.

# EXAMPLES

Create a predicate file for a four-taxon sample:

```sh
printf 'A B | C D\n\nA C | B D\n' > predicates.txt
```

If `sample.trees` contains eight copies of `((A,B),C,D);` and two of
`((A,C),B,D);`, their supports are 0.8 and 0.2. Select the first predicate:

```sh
partitions-supported predicates.txt sample.trees --above 0.5 > supported.txt
```

Select intermediate support, or the complement of high support:

```sh
partitions-supported predicates.txt sample.trees --above 0.1 --below 0.9
partitions-supported predicates.txt sample.trees --above 0.5 --not
```

Read a sample from stdin, then examine the event history of the selected predicates:

```sh
partitions-supported predicates.txt --above 0.5 < sample.trees > supported.txt
trees-to-SRQ supported.txt sample.trees --skip 0 --mode values
```

# SEE ALSO

**trees-consensus**(1), **trees-bootstrap**(1), **trees-to-SRQ**(1)

# REPORTING BUGS

BAli-Phy online help: <https://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
