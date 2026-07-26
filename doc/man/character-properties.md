% character-properties(1)
% Benjamin Redelings
% Jul 2026

# NAME

**character-properties** - Summarize and report sampled character properties.

# SYNOPSIS

**character-properties summarize** [OPTIONS] _C1.propertiesN.json_ [...]

**character-properties report** _SUMMARY_ _ALIGNMENT_ **--alphabet** _ALPHABET_
**--property** _NAME_ **--kind** _KIND_ [OPTIONS]

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
Exact medians are computed by rereading the input streams in memory-bounded
character blocks.

The **report** command projects one summarized property onto a template
alignment. Each stored value continues to identify an observed character by
sequence name and ungapped character index; a template column is only a
presentation group. One representative character is selected per nonempty
column, so characters that share a column do not appear as separate ranked
sites.

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

# REPORT OPTIONS

**--alphabet=ALPHABET**
: Alphabet used to interpret one logical character in the template alignment.
  Codon reports use its genetic code to include amino-acid translations.

**--property=NAME**
: Property to report.

**--kind=property|positive-selection**
: Use a generic property report or the specialized positive-selection report.
  The latter accepts `posSelection` and names ending in `-posSelection`, ranks
  columns by their largest probability, and includes the matching `dNdS`
  property when present.

**--format=text|tsv|json**
: Output format (default: text). Text and TSV coordinates are one-based for
  readers; versioned JSON coordinates are zero-based.

**--sort=column|mean-ascending|mean-descending|sd-descending**
: Order generic report rows. The representative for each column is the
  character with the corresponding extreme value. The default is column order;
  positive-selection reports default to descending probability.

**--minimum-probability=P**
: Omit positive-selection columns whose representative probability is below
  _P_ (default: 0.5).

# VALIDATION

Iterations must be nonnegative and strictly increasing within each chain.
Retained samples must have identical property names, sequence names, and
ungapped character counts. Every observed character must have a category/state
pair in every retained sample. Category/state indices, property-table bounds,
finite values, and cross-chain shapes are validated before a result is emitted.

# EXAMPLE

```
character-properties summarize \
  run-1/C1.properties1.json run-2/C1.properties1.json \
  --skip=1000 --subsample=2 > P1.character-properties.json
```

Create a text report of columns with posterior positive-selection probability
at least 0.95:

```
character-properties report P1.character-properties.json P1.initial.fasta \
  --alphabet 'Codons(DNA,standard)' --property posSelection \
  --kind positive-selection --minimum-probability 0.95
```
