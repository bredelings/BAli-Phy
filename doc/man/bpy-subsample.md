% bpy-subsample(1)
% Benjamin Redelings
% October 2026

# NAME

**bpy-subsample** - Retain regularly spaced lines from a text stream.

# SYNOPSIS

**bpy-subsample** \[OPTIONS\] [_FACTOR_] < _INPUT_ > _OUTPUT_

# DESCRIPTION

Read text from standard input and write selected lines to standard output. Use this
to thin a table or a tree sample containing one tree per line. Blank lines count as
lines; the tool does not recognize multiline records. Use **cut-range** for sampled
alignments or other records marked by iteration values.

Selection proceeds in this order: preserve an optional header, skip initial data
lines, subsample the remaining lines, and stop at the output limit. The first data
line after skipping is always selected unless the output limit is zero.

# ARGUMENTS AND OPTIONS

**-h**, **--help**
: Print help and exit.

**_FACTOR_**
: Optional positional subsampling factor. Default: 1.
  An integer factor N keeps data lines 1, 1+N, 1+2N, and so on after skipping.
  Fractional factors are allowed: 1.5 keeps lines 1, 3, 4, 6, 7, 9, and so on.
  The factor must be finite and positive. Factors below 1 keep every line.

**--skip** _N_
: Discard the first N data lines, after any preserved header. Default: 0.

**--max** _N_
: Maximum number of data lines to write. Omission means no limit; zero writes no
  data lines. A preserved header does not count toward this limit.

**--header**
: Copy the first input line before applying selection to the remaining data lines.
  The header does not count toward **--skip** or **--max**. Empty input produces
  no output.

# EXAMPLES

Keep the first tree and every tenth tree thereafter from a one-tree-per-line file:

    bpy-subsample 10 < C1.trees > C1.thinned.trees

Preserve a table header, skip 100 data rows, then keep at most 500 rows spaced five
rows apart:

    bpy-subsample 5 --header --skip=100 --max=500 < C1.tsv > selected.tsv

# EXIT STATUS

Returns 0 on success (including help) and 1 on an error.

# SEE ALSO

**cut-range**(1), **stats-cat**(1), **statreport**(1)

# REPORTING BUGS

BAli-Phy online help: <https://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
