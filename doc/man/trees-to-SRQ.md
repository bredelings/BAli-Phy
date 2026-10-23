% trees-to-SRQ(1)
% Benjamin Redelings
% Feb 2018

# NAME

**trees-to-SRQ** - Generate Scaled Regeneration Quantile (SRQ) plot

# SYNOPSIS

**trees-to-SRQ** _predicates-file_ _sampled-trees_ [_sampled-trees_ ...]

# DESCRIPTION

Generate coordinates for Scaled Regeneration Quantile (SRQ) plots, showing when a sampled
sequence of trees returns to an event described in the predicates file. Each curve is written
as two columns, with a blank line between curves.

In **SRQ** mode, the horizontal axis numbers the returns to the event and the vertical axis
gives their positions in the tree sample. The curve also includes the start and end of the
sample. In **sum** mode, the horizontal axis is the number of trees examined and the vertical
axis counts how many satisfy the event. By default, each axis is divided by its final value;
a cumulative count of zero remains zero throughout the curve.

Point selection uses these normalized coordinates even when output scaling is disabled.
**--max-points** controls the approximate density of points, rather than imposing a strict
limit. The first and last points are always retained. In **values** mode, the program instead
writes one row per sampled tree, with a column of zeros and ones for each predicate;
point selection and axis scaling do not apply.

# ALLOWED OPTIONS:
**-h**, **--help**
: produce help message

**--files** _arg_
: tree samples to examine

**--predicates** _arg_
: predicates to examine

**-s** _arg_ (=10%), **--skip** _arg_ (=10%)
: Number of initial trees to discard from every run. A percentage of the
  shortest run determines one common count, after **--until** and before **--subsample**.

**-x** _arg_ (=1), **--subsample** _arg_ (=1)
: factor by which to sub-sample

**-u** _arg_, **--until** _arg_
: Read until this number of trees.

**--max-points** _arg_ (=1000)
: Approximate number of points per curve.

**--mode** _arg_ (=SRQ)
: SRQ, sum, or values

**--invert**
: consider the inverse of each event instead

**--no-scale-x**
: Show the horizontal axis in counts rather than as a fraction of its final value.

**--no-scale-y**
: Show the vertical axis in counts rather than as a fraction of its final value.


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

