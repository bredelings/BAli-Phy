% statreport(1)
% Benjamin Redelings
% Feb 2018

# NAME

**statreport** - Compute summary statistics for tab-delimited data files.

# SYNOPSIS

**statreport** [OPTIONS] file1 [file2 file3 ... ]

# DESCRIPTION

Compute summary statistics for tab-delimited data files.

Fields whose final component begins with `LogOdds`, ignoring case, also receive a
`posterior-probability` summary. Components are separated by `/` or `:`, so generated names such as
`P1/M8a_test:LogOddsPosSelection` are recognized. This summary averages the probabilities represented
by the sampled log odds and reports both the resulting probability and its log odds; ordinary
summaries of the sampled log odds are retained.

# ALL OPTIONS:
**--condition** _key=value_
: Summarize retained samples whose numeric field _key_ equals _value_. The condition is applied
  after the usual burn-in, endpoint, and subsampling choices. The condition field is read even
  when it is not selected for output. Matching counts are reported for each chain and pooled.
  ACT, ESS, burn-in estimation, and PSRF are disabled for conditioned summaries. Empty matching
  samples are reported explicitly; they are not errors. Only one condition is supported.

**-h**, **--help**
: Produce help message.

**-V**, **--verbose**
: Output more log messages on stderr.

**-s** _arg_ (=10%), **--skip** _arg_ (=10%)
: Number of initial lines to skip.

**-x** _arg_ (=1), **--subsample** _arg_ (=1)
: Factor by which to sub-sample. Reported burn-in values remain in original input-line units.

**-u** _arg_, **--until** _arg_
: Read up to this iteration.

**-I** _arg_, **--ignore** _arg_
: Do not analyze these fields.

**-S** _arg_, **--select** _arg_
: Analyze only these fields.

**-i**, **--individual**
: Show results for individual files separately also.

**--truth** _arg_
: True value

**--min** _arg_
: Required minimum number of lines to read.

**--mean**
: Show mean and standard deviation.

**--mode**
: Show mode (with precision)

**--median**
: Show median and confidence level.

**--confidence** _arg_ (=0.95)
: Confidence interval levels (colon-separated).

**--BCI** _arg_ (=HPD)
: Type of Bayesian Credible Interval (BCI): HPD or central

**-p** _arg_ (=4), **--precision** _arg_ (=4)
: Number of significant figures.


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
