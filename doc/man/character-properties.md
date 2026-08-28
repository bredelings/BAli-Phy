% character-properties(1)
% Benjamin Redelings
% Aug 2026

# NAME

**character-properties** - Summarize and report sampled character properties.

# SYNOPSIS

**character-properties summarize** [OPTIONS] _C1.PN.site-property-samples.jsonl_ [...]

**character-properties report** [OPTIONS] _SUMMARY_ _ALIGNMENT_ _PROPERTY_

**character-properties positive-selection** [OPTIONS] _SUMMARY_ _ALIGNMENT_ [_PROPERTY_]

# DESCRIPTION

The **summarize** command reads one JSON Lines property stream per MCMC chain
and computes the posterior mean, standard deviation, and median of every named
property for each ungapped observed sequence character. A sampled
category/state pair selects a value from that sample's property table.

The command writes one versioned JSON document to standard output. Property
means, population posterior standard deviations, and exact empirical medians
are keyed by sequence name and use zero-based
`ungapped-sequence-character` coordinates. Samples from all input chains are
pooled by draw; chains are not weighted equally after taking separate means.

For an even number of observations, the empirical median is the lower of the
two central observed values. This definition is preserved by monotone
transformations: the median log-rate is the logarithm of the median rate.
Exact medians are computed in memory-bounded character blocks by replaying the
file prefixes observed during the initial moments pass. Records appended after
that pass are ignored, so summarization can run while MCMC sampling continues.
The command stops with an error if an observed prefix is subsequently modified,
replaced with different contents, or truncated.

The two report commands project summarized letter properties onto a template
alignment. Each stored value continues to identify a non-gap observed letter
by sequence name and ungapped character index; an alignment column is only a
presentation group.

The ordinary **report** command either describes every nonempty column or
selects letters globally before grouping the selected letters by column. An
above or highest selection uses the highest-scoring selected letter as the
column representative. A below or lowest selection uses the lowest-scoring
selected letter. Row ordering is applied only after representative selection.

With no selection option, **report** prints the minimum, lower middle, and
maximum of both the per-letter posterior means and per-letter posterior medians
in every column. These column-level middle values are distinct from the
posterior median stored for each individual letter.

The **positive-selection** command selects letters using their posterior mean
`posSelection` probability, groups them by column, and reports the
highest-probability letter in each resulting column. The matching dN/dS
statistics describe that representative but do not help choose it. The default
property is `posSelection`; names ending in `-posSelection` use the corresponding
`-dNdS` companion when it exists.

# SUMMARIZE OPTIONS

**--skip=ITER**
: Discard records at or before iteration _ITER_.

**--until=ITER**
: Discard records after iteration _ITER_.

**--subsample=N**
: After applying the iteration bounds, retain the first eligible stored record
  and every _N_th eligible record thereafter, independently in each chain.

**--median-memory=MIB**
: Target _MIB_ mebibytes of working memory for exact median calculation
  (default: 256). This excludes each decoded input record and the final summary
  arrays.

# COMMON REPORT OPTIONS

**--alphabet=ALPHABET**
: Constrain the alphabet used to interpret logical alignment characters. When
  omitted, **report** guesses the alphabet from the alignment, while
  **positive-selection** defaults to **Codons**. Partial names are completed
  from the data: **Codons** guesses DNA versus RNA and uses the standard genetic
  code, while **Codons(,mt-vert)** also guesses DNA versus RNA but preserves the
  specified genetic code.

**--format=text|tsv**
: Output format (default: text). Positive-selection text reports show the
  representative codon and amino acid, probability, posterior dN/dS mean and
  standard deviation, and source letter, using three decimal places. TSV keeps
  the complete posterior summaries and separate provenance fields. Both formats
  use one-based coordinates for readers.

**--sort=column|increasing|decreasing**
: Order completed rows (default: column). Increasing and decreasing use the
  selected representative score. For an all-column ordinary report, they use
  the column's middle value for the statistic selected by **--by**. Equal
  values have no defined secondary order.

**--condition=NAME**
: Use only samples in which the named Boolean model condition is true.

# ORDINARY REPORT OPTIONS

**--above=VALUE**
: Select letters whose score is strictly greater than _VALUE_.

**--below=VALUE**
: Select letters whose score is strictly less than _VALUE_.

**--highest[=PERCENT]**
: Select the highest-scoring percentage of projected non-gap letters. The
  implicit percentage is 1%.

**--lowest[=PERCENT]**
: Select the lowest-scoring percentage of projected non-gap letters. The
  implicit percentage is 1%.

**--by=mean|median**
: Use the per-letter posterior mean or posterior median for selection,
  representative choice, and value ordering (default: mean). Representative
  rows display all available posterior summaries regardless of this choice.

The four selection options are mutually exclusive. Percentage selection first
sorts the _N_ projected non-gap letters and takes exactly
`max(1, floor(N*PERCENT/100))`. Equal scores do not enlarge the selection and
have no defined secondary order. Multiple selected letters can subsequently
collapse into one alignment-column row.

# POSITIVE-SELECTION OPTIONS

**--above=PROBABILITY**
: Select letters whose posterior mean probability is strictly greater than
  _PROBABILITY_ (default: 0.5).

**--highest[=PERCENT]**
: Instead select the highest-probability percentage of letters, with an
  implicit percentage of 1%.

**--unconditional**
: Use the model-averaged posterior. By default, positive-selection reports use
  samples where `positiveSelectionInModel` is true. If that conditioned view
  is absent, the command reports an error rather than silently changing the
  scientific quantity.

# VALIDATION

Iterations must be nonnegative and strictly increasing within each chain.
Only newline-terminated JSON Lines records are committed to the initial file
snapshot; an unterminated record being written at the end of a file is ignored.
Retained samples must have identical property names, sequence names, and
ungapped character counts. Every observed character must have a category/state
pair in every retained sample. Category/state indices, property-table bounds,
finite values, and cross-chain shapes are validated before a result is emitted.

Positive-selection properties and probability thresholds must lie in `[0,1]`.
A conditioned view with no true samples cannot produce a report.

# EXAMPLES

Summarize two chains:

```
character-properties summarize \
  run-1/C1.P1.site-property-samples.jsonl run-2/C1.P1.site-property-samples.jsonl \
  --skip=1000 --subsample=2 > P1.site-property-summary.json
```

Describe every alignment column for a rate property:

```
character-properties report \
  P1.site-property-summary.json P1.initial.fasta rate
```

Select letters whose posterior median rate exceeds 2 and order the resulting
column representatives from high to low:

```
character-properties report summary.json alignment.fasta rate \
  --above=2 --by=median --sort=decreasing
```

Report the highest 1% of letters, always retaining at least one:

```
character-properties report summary.json alignment.fasta rate --highest
```

Report positive selection conditional on its presence in the model:

```
character-properties positive-selection \
  P1.site-property-summary.json P1.initial.fasta --above=0.95
```

Report model-averaged foreground positive selection using the vertebrate
mitochondrial genetic code:

```
character-properties positive-selection summary.json alignment.fasta \
  foreground-posSelection --unconditional --alphabet='Codons(,mt-vert)'
```
