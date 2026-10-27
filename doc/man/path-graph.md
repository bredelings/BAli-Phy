% path-graph(1)
% Benjamin Redelings
% October 2026

# NAME

**path-graph** - Count pairwise alignment-path edges across sampled alignments.

# SYNOPSIS

**path-graph** **`--taxa`** _names_ \[OPTIONS\] < _alignments-file_

# DESCRIPTION

Summarize how the same sequences are aligned across a sample of alternative alignments.
For each requested pair of sequences, represent each alignment as a path and count how
many retained alignments contain each path edge. These counts describe support for local
alignment choices, including aligned positions and gaps.

A path starts at `(0, 0)`. Its coordinates count positions consumed in the two sequences.
A horizontal step advances only the first sequence, a vertical step only the second, and
a diagonal step advances both, representing aligned positions. Columns with gaps in both
selected sequences contribute no step. Coordinates count alphabet units, so a codon is
one position when using a codon alphabet.

Before counting, the command consistently orders adjacent columns that share no nongap
sequence entries, without changing the order of positions within any sequence. Thus the
path can differ from one obtained by reading the input columns literally. This reduces
differences caused solely by the order of independent columns.

# INPUT

Read a sample of FASTA alignments from standard input, with a blank line between
alignments. All alignments must contain the same named sequences with the same underlying
sequence content; gap placement may vary. Sequence rows may appear in different orders:
they are matched by name. Supply at least two names with **`--taxa`** to obtain a pairwise
report; all pairs among the supplied names are reported.

The command skips the requested initial samples, then thins the remaining sample if
necessary to meet **`--max-alignments`**. It does not simply stop reading at that limit.
If a later alignment cannot be read, the loader can warn and continue with the alignments
already loaded. Check standard error for such warnings before interpreting the output.

# OPTIONS

**-h**, **`--help`**
: Print help and exit.

**`--taxa`** _names_
: Required colon-separated sequence names, for example `human:chimp:gorilla`.
  Report every pair among these names.

**`--alphabet`** _name_
: Specify the alphabet instead of inferring it from the first alignment after skipping.
  Examples include `DNA`, `RNA`, `Amino-Acids`, `Amino-Acids+stop`, `Triplets`, `Codons`,
  and `Codons+stop`.

**`--skip`** _number_
: Number of initial alignment samples to skip (nonnegative; default: 0).

**`--max-alignments`** _number_
: Maximum number of retained alignments (positive; default: 1000). Thin across the input
  to stay within this limit. Use `-1` to retain all alignments after skipping.

# OUTPUT

Standard output contains one block per sequence pair. Its first line is:

```
number-of-alignments number-of-edges x-sequence y-sequence
```

The names identify the coordinate axes; do not assume that their order matches the order
in **`--taxa`**. The alignment count is the number retained after skipping and thinning.
The edge count gives the number of following rows, each containing:

```
x1 y1 x2 y2 count
```

The edge goes from `(x1, y1)` to `(x2, y2)`. Its count is the number of retained alignments
containing that edge. Divide by the block's alignment count to obtain its frequency.
Counts are not normalized, and frequencies of all edges do not sum to one: each alignment
contributes multiple edges. Rows are sorted by their four coordinates. Blocks follow one
another without blank separators.

Progress messages, warnings, and errors go to standard error.

# EXAMPLE

Count paths for all three pairs after skipping 100 samples:

```bash
path-graph --taxa=human:chimp:gorilla --alphabet=DNA --skip=100 \
    --max-alignments=1000 < alignments.fasta > paths.txt
```

# SEE ALSO

**alignment-consensus**(1), **alignment-info**(1)

# REPORTING BUGS

BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
