% trees-distances(1)
% Benjamin Redelings
% Feb 2018

# NAME

**trees-distances** - Compute autocorrelations or other functions of tree distances.

# SYNOPSIS

**trees-distances** _analysis_ trees-file1 [trees-file2 ...]

# DESCRIPTION

Compute autocorrelations or other functions of tree distances.

# INPUT OPTIONS:
**-h**, **--help**
: produce help message

**-s** _arg_ (=0), **--skip** _arg_ (=0)
: number of tree samples to skip

**-u** _arg_, **--until** _arg_
: Read until this number of trees.

**-m** _arg_, **--max** _arg_
: Thin tree samples down to this number of trees.

**-x** _arg_ (=1), **--subsample** _arg_ (=1)
: factor by which to subsample

**-V**, **--verbose**
: Output more log messages on stderr.


# ANALYSIS OPTIONS:
**--analysis** _arg_ (=matrix)
: Analysis: matrix, autocorrelation, diameter, compare, closest, convergence, converged.

**--metric** _arg_ (=topology)
: Tree distance: topology, branch, internal-branch.

**--remove-duplicates**
: \[matrix\]: disallow zero distances  between points.

**--max-lag** _arg_
: \[autocorrelation\]: max lag to consider.

**--CI** _arg_ (=0.95)
: Confidence interval size.

**--converged** _arg_ (=0.05)
: Comma-separated quantiles of distance required for converged? (smaller is more strict).

**--mean**
: Show mean and standard deviation

**--median**
: Show median and confidence interval

**--minmax**
: Show minimum and maximum distances

**--leaves-only**
: Show minimum and maximum distances

**--topology-only**
: Show minimum and maximum distances

**--jitter** _arg_
: Amount of noise to add to distances


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

