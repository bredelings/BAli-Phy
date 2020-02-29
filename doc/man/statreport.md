% statreport(1)
% Benjamin Redelings
% Feb 2018

# NAME

**statreport** - Compute summary statistics for tab-delimited data files.

# SYNOPSIS

**statreport** [OPTIONS] file1 [file2 file3 ... ]

# DESCRIPTION

Compute summary statistics for tab-delimited data files.

# ALL OPTIONS:
**-h**, **--help**
: Produce help message.

**-V**, **--verbose**
: Output more log messages on stderr.

**-s** _arg_ (=10%), **--skip** _arg_ (=10%)
: Number of initial lines to skip.

**-x** _arg_ (=1), **--subsample** _arg_ (=1)
: Factor by which to sub-sample.

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

**--log-mean**
: Show log mean of X given log X.

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

