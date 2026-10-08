% alignment-compare(1)
% Benjamin Redelings
% October 2026

# NAME

**alignment-compare** - Compare two alignment distributions at each residue.

# SYNOPSIS

**alignment-compare** \[OPTIONS\] _sample-file1_ _sample-file2_ _target-alignment_

# DESCRIPTION

Compare the residue-homology distributions in two alignment samples and write agreement scores
arranged according to a target alignment. The target supplies the layout of the output; it
need not be a member of either sample. Scores go to standard output and diagnostics to
standard error.

# INPUT

All three filenames are required positional arguments. Each sample file contains one or more
FASTA alignments separated by empty lines. The third file contains the target alignment; use
`-` in that position to read it from standard input. Sample filenames do not interpret `-`
as standard input.

Use samples and a target for the same sequences. Sequence names must be unique within each
alignment, and names and ungapped lengths must match; row order may differ.
Residues are identified by sequence name and ungapped position,
not by comparing their letters. BAli-Phy internal-node placeholder sequences are removed
before comparison.

Retained sample alignments cannot contain `?` or `=`, which leave gap/residue status unknown.
Ambiguous but present residues, such as DNA `N` or `R`, are allowed. Sample thinning is applied
before these retained-alignment checks.

# OUTPUT

The first line lists target sequence names separated by spaces. Each following line corresponds
to one target alignment column and contains one score per sequence, in target row order,
followed by an additional value of `1` for the column.

For each residue and each other sequence, the tool compares the two empirical distributions of
its aligned partner, including the possibility of alignment to a gap. It computes their total
variation distance (half the sum of absolute probability differences), takes the largest
such distance across other sequences, and reports one minus that distance.

A score of `1` means agreement between the samples for that residue; smaller scores indicate
larger differences, down to `0`. These are agreement scores, not posterior probabilities that
the target alignment is correct. Target positions without a present residue receive `1`.
The output can be supplied as an AU file to **alignment-draw**(1).

# OPTIONS

**-h**, **`--help`**
: Print usage and options, then exit.

**`--alphabet`** _name_
: Specify the alphabet instead of inferring it: `DNA`, `RNA`, `Amino-Acids`, `Amino-Acids+stop`,
  `Triplets`, `Codons`, or `Codons+stop`.

**`--seed`** _integer_
: Set the random seed.

**`--max-alignments`** _count_ (=1000)
: Maximum retained alignments per sample. Samples are thinned to this limit rather than
  truncated to their first alignments; `-1` means unlimited.

**`--verbose`**
: Output more log messages on standard error, including the seed and sample sizes.

# EXAMPLES

Compare two samples and lay out scores on a target alignment:

```sh
alignment-compare sample1.fastas sample2.fastas target.fasta > agreement.prob
alignment-draw target.fasta --AU agreement.prob > agreement.html
```

Read the target from standard input and retain all sample alignments:

```sh
alignment-compare --max-alignments=-1 sample1.fastas sample2.fastas - < target.fasta
```

# SEE ALSO

**alignment-draw**(1), **alignment-info**(1)

# REPORTING BUGS

BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
