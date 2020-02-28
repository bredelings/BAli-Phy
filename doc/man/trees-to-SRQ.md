% tree-to-srq(1)
% Benjamin Redelings
% Feb 2018

# NAME

**tree-to-srq** - Generate Scaled Regeneration Quantile (SRQ) plot

# SYNOPSIS

**tree-to-srq** _predicates-file_ _sampled-trees_ [_sampled-trees_ ...]

# DESCRIPTION

Generate Scaled Regeneration Quantile (SRQ) plot

# ALLOWED OPTIONS:
**-h**, **--help**
: produce help message

**--files** _arg_
: tree samples to examine

**--predicates** _arg_
: predicates to examine

**-s** _arg_ (=10%), **--skip** _arg_ (=10%)
: Number of trees to skip.

**-x** _arg_ (=1), **--subsample** _arg_ (=1)
: factor by which to sub-sample

**-u** _arg_, **--until** _arg_
: Read until this number of trees.

**--max-points** _arg_
: maximum number of points to record

**--mode** _arg_ (=SRQ)
: SRQ, sum, or values

**--invert**
: consider the inverse of each event instead

**--no-scale-x**
: don't scale X

**--no-scale-y**
: don't scale Y


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

