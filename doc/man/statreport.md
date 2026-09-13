% statreport(1)
% Benjamin Redelings
% Feb 2018

# NAME

**statreport** - Summarize numeric samples from MCMC logs.

# SYNOPSIS

**statreport** \[OPTIONS\] _file1_ [_file2_ ...]

# DESCRIPTION

**statreport** computes summary statistics and sampling diagnostics for numeric fields in MCMC
logs. It accepts tab-delimited files with a header row of column names, or BAli-Phy JSON logs.
Use **-** as a filename to read standard input. Results are written to the terminal.

Supply multiple files to summarize independent runs of the same analysis. The selected columns
must have matching names and order. Retained samples are pooled, so longer retained runs
contribute more samples to the combined summaries.

By default, the report gives the median and a 95% highest-posterior-density credible interval for
each parameter, together with applicable autocorrelation, effective sample size, and convergence
diagnostics. The initial 10% of the shortest input is discarded as burn-in.

# EXAMPLES

Summarize a single run:

    statreport run-1/C1.log

Combine independent runs:

    statreport run-1/C1.log run-2/C1.log

Discard the first 1000 data rows from each file:

    statreport --skip=1000 run-1/C1.log

Show both means and medians:

    statreport --mean --median run-1/C1.log

Analyze only columns 2 through 4:

    statreport --select=2:4 run-1/C1.log

Row counts exclude the header. They correspond to MCMC iterations only when one row was logged
per iteration.

# OPTIONS

## Sample selection

**`-s`** _COUNT|PERCENT_, **`--skip`** _COUNT|PERCENT_
: Discard initial samples as burn-in. Accepts a data-row count or a percentage such as `10%`
  (the default). A percentage is calculated from the shortest input after endpoint limiting and
  subsampling; the same number of retained rows is then removed from each file. With subsampling,
  an integer count is divided by the subsampling factor and rounded down before rows are removed.

**`-u`** _NUM_, **`--until`** _NUM_
: Use only the first NUM data rows of each file, before subsampling and burn-in removal.
  This is a row limit, not a cutoff on the value of an iteration field. By default, read to the end.

**`-x`** _NUM_, **`--subsample`** _NUM_
: Read every NUMth data row, starting with the first. The default is 1 (no thinning).
  Burn-in removal follows this selection.

**`--min`** _NUM_
: Require at least NUM rows in each file after endpoint limiting and subsampling, but before
  burn-in removal. By default, no additional minimum is imposed.

## Field selection

**`-S`** _FIELD|RANGE_, **`--select`** _FIELD|RANGE_
: Analyze only the specified field name or inclusive column range, such as `2:4`.
  Column numbers start at 1. Repeat the option to select multiple fields or ranges.
  Without this option, analyze all fields not excluded by **`--ignore`**.

**`-I`** _FIELD|RANGE_, **`--ignore`** _FIELD|RANGE_
: Exclude a field name or column range. Repeat to exclude multiple fields or ranges.
  Exclusion takes precedence over selection. Fields chosen for analysis must be numeric.

## Summaries and formatting

**`--mean`**
: Show the mean and standard deviation. Suppresses the default median summary unless
  **`--median`** is also supplied.

**`--median`**
: Show the median and credible interval. This is the default unless **`--mean`** is supplied.

**`--mode`**
: Add an estimated mode and a measure of its resolution. See INTERPRETING THE OUTPUT.

**`--confidence`** _PROBABILITIES_
: Credible-interval probabilities, separated by colons, such as `0.8:0.95`.
  The default is `0.95`. These control the intervals accompanying median summaries.

**`--BCI`** _TYPE_
: Credible-interval type: `HPD` (the default) or `central`. HPD uses a shortest interval containing
  the requested posterior probability; central intervals leave equal probability in each tail.

**`-i`**, **`--individual`**
: Include per-file summaries and diagnostics in addition to pooled results. The mode estimate
  and comparisons with **`--truth`** remain pooled in ordinary, unconditioned reports.

**`-p`** _NUM_, **`--precision`** _NUM_
: Number of significant figures in the output. The default is 4.

## Specialized analysis

**`--condition`** _FIELD=VALUE_
: Summarize only retained samples whose numeric FIELD equals VALUE. VALUE must be finite.
  Only one condition is supported. See SPECIALIZED SUMMARIES for matching counts and diagnostic
  limitations.

**`--truth`** _VALUE_
: Compare samples with a known numeric value, reporting bias, the fraction above VALUE minus
  one half (`high`), mean absolute error (`absE`), and root mean square error (`rmsE`).
  The same reference value is used for every analyzed field.

## General options

**`-h`**, **`--help`**
: Print usage information.

**`-V`**, **`--verbose`**
: Print additional diagnostic messages to standard error.

# INTERPRETING THE OUTPUT

A line such as `parameter ~ 1.2 (0.8,1.7) @ 95%` gives the median and credible interval.
An interval is printed as `(NA,NA)` when the sample count is too small for the requested
probability: the count multiplied by one minus that probability must be at least 10.

With **`--mean`**, `E parameter = ... [+- ...]` gives the mean and standard deviation, not the
standard error of the mean. With **`--mode`**, `parameter ^ ... [+- ...]` gives the midpoint and
full width of a narrow sample interval used to estimate the mode. This width is a resolution
measure, not a credible interval or standard error. The mode is `NA` with fewer than 20 samples.

Constant fields are reported as single values. In ordinary reports, fields that only increase
or only decrease are marked accordingly instead of receiving the usual summaries.

## Sampling and convergence diagnostics

**`t @`**
: Estimated autocorrelation time, in retained-sample units. Larger values indicate more
  dependence between successive samples.

**`Ne`**
: Effective sample size: the retained sample count divided by the estimated autocorrelation
  time. This is an estimate of sampling information, not a count of independent observations.

**`burnin`**
: A heuristic estimate based on repeated crossings of the range between the 5th and 95th
  percentiles of the final third of each input. The pooled report gives the largest estimate
  among runs. `Not Converged!` means a run did not complete those crossings before its final
  third. The estimate uses data before burn-in removal and is reported in original data-row
  units, even with subsampling. It does not change which samples **`--skip`** removes.

With multiple files, two additional measures compare chains using central 80% intervals,
regardless of the chosen credible-interval options:

**`PSRF-80%CI`**
: Width of the pooled interval divided by the average within-chain interval width.
  For integer-valued fields, widths count integer values, including both endpoints.

**`PSRF-RCF`**
: For each chain's interval, the fraction of that chain's samples inside it divided by the
  fraction of pooled samples inside it, averaged over chains.

Values near 1 indicate agreement by these measures; larger values indicate differences between
chains. These are interval-based diagnostics, not modern R-hat. Neither they nor the burn-in
estimate establish convergence on their own.

# SPECIALIZED SUMMARIES

## Conditional summaries

**`--condition`** filters samples after endpoint limiting, subsampling, and burn-in removal.
The condition field is read even when it is not selected for output. Matching sample counts are
reported for each chain and pooled. No matching samples is reported explicitly and is not an error.

Conditional reports omit autocorrelation times, effective sample sizes, burn-in estimates, and
PSRF measures: filtering changes the time sequence to which those diagnostics apply.
Use the ordinary report to examine diagnostics for unfiltered parameter samples.

## Log-odds summaries

Fields whose final name component begins with `LogOdds`, ignoring case, also receive a
`posterior-probability` summary. Components are separated by `/` or `:`, so names such as
`P1/M8a_test:LogOddsPosSelection` are recognized.

The summary converts each sampled natural-log odds to a probability, averages those probabilities,
and reports both the average probability and its log odds. This differs from converting the
average log odds to a probability. Ordinary summaries of the sampled log odds are retained.

# REPORTING BUGS

BAli-Phy online help: <https://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
