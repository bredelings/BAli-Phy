% cut-range(1)
% Benjamin Redelings
% October 2026

# NAME

**cut-range** - Select complete records by their logged key values.

# SYNOPSIS

**cut-range** \[OPTIONS\] [_FILE_ ...]

# DESCRIPTION

Select records from text files using a numeric key, usually the iteration number
recorded with each sampled alignment. Each marker line and the following lines,
up to the next marker, form one record. Selected records are copied to standard
output in input-file order.

The default marker contains the exact text `iterations = `, followed by a number.
The spaces around `=` matter; the marker need not begin the line. **--key** changes
the key name. Key values must be in ascending order: reading a file stops at the
first value above the upper bound.

Bounds and subsampling apply independently to every file. Text before the first
marker is copied when **--skip** is omitted and discarded when it is supplied.
With no filenames, read standard input; **-** also denotes standard input.

# OPTIONS

**-h**, **--help**
: Print help and exit.

**--key** _NAME_
: Select using markers containing `NAME = ` followed by a number.
  Default: `iterations`.

**--skip** _N_
: Minimum key value to retain, inclusive. By default, there is no lower bound.
  This is a key-value cutoff, not a number of records to discard.

**--until** _N_
: Maximum key value to retain, inclusive. By default, there is no upper bound.

**--size** _N_
: Key-value span to retain. The inclusive upper bound is _skip_ + N, using zero
  when **--skip** is omitted. For example, **--skip=100 --size=20** retains keys
  from 100 through 120. This limits the range of key values, not the number of
  records. Cannot be combined with **--until**.

**--subsample** _N_
: Keep the first eligible record and every Nth eligible record thereafter,
  restarting for each file. N must be at least 1. Default: 1.

**--verbose**
: Write additional diagnostic messages to standard error.

# EXAMPLES

Select sampled alignments with iteration values from 100 through 1000:

    cut-range --skip=100 --until=1000 run-1/C1.P1.fastas > selected.fastas

From each of two runs, retain every tenth eligible alignment starting at
iteration 100:

    cut-range --skip=100 --subsample=10 run-1/C1.P1.fastas run-2/C1.P1.fastas > pooled.fastas

# EXIT STATUS

Returns 0 on success (including help) and 1 on an error.

# SEE ALSO

**bpy-subsample**(1), **alignment-chop-internal**(1), **alignment-max**(1)

# REPORTING BUGS

BAli-Phy online help: <https://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
