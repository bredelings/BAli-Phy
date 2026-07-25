% calc-properties(1)
% Benjamin Redelings
% Jul 2026

# NAME

**calc-properties** - Summarize sampled character properties.

# SYNOPSIS

**calc-properties** [OPTIONS] _C1.propertiesN.json_ [...]

# DESCRIPTION

Read one JSON Lines property stream per MCMC chain and compute the posterior
mean, standard deviation, and median of every named property for each ungapped
sequence character. A sampled category/state pair selects a value from that
sample's property table. Missing category/state draws are omitted and recorded
through a per-character count.

The command writes one versioned JSON document to standard output. Property
means, population posterior standard deviations, exact empirical medians, and
counts are keyed by sequence name and use zero-based
`ungapped-sequence-character` coordinates. Samples from all input chains are
pooled by draw; chains are not weighted equally after taking separate means.

For an even number of observations, the empirical median is the lower of the
two central observed values. This definition is preserved by monotone
transformations: the median log-rate is the logarithm of the median rate.
Exact medians are computed by rereading the input streams in memory-bounded
character blocks.

# OPTIONS

**--skip=ITER**
: Discard records at or before iteration _ITER_.

**--until=ITER**
: Discard records after iteration _ITER_.

**--subsample=N**
: After applying the iteration bounds, retain the first eligible stored record
  and every _N_th eligible record thereafter, independently in each chain.

**--median-memory=MIB**
: Target _MIB_ mebibytes of working memory for exact median calculation
  (default: 256). This excludes the Python runtime, each decoded input record,
  and the final summary arrays.

# VALIDATION

Iterations must be nonnegative and strictly increasing within each chain.
Retained samples must have identical property names, sequence names, and
ungapped character counts. Category/state indices, property-table bounds,
finite values, and cross-chain shapes are validated before a result is emitted.

# EXAMPLE

```
calc-properties run-1/C1.properties1.json run-2/C1.properties1.json \
  --skip=1000 --subsample=2 > P1.character-properties.json
```
