% alignment-max(1)
% Benjamin Redelings
% October 2026

# NAME

**alignment-max** - Construct a posterior decoding alignment from an alignment sample.

# SYNOPSIS

**alignment-max** \[OPTIONS\] < _SAMPLES_

# DESCRIPTION

Construct a representative alignment from a sample of alternative alignments of the same
sequences, such as a sample produced by BAli-Phy. This is called *posterior decoding*: the sample
is used to estimate support for alignment columns, and the program chooses a combination of
columns with the best overall score while preserving sequence order. The result can combine
columns from different samples; it need not be one of the input alignments.

A column's support is the fraction of retained samples in which it occurs. By default, the score
favors columns that have both high support and many aligned residue pairs. Here a residue is a
sequence unit, such as a nucleotide or amino acid. A residue pair is two residues from different
sequences placed in the same column. Other scoring choices are described below.

Use this tool when you want one complete alignment summarizing a sample. To retain relationships
according to a chosen support threshold, use **alignment-consensus**(1). To assess support for
individual residues in an existing alignment, use **alignment-gild**(1).

# INPUT AND OUTPUT

Read FASTA alignments separated by empty lines from standard input. Sequence names must match,
although their order may differ. For each sequence, its length after removing gaps must be the
same in every sample. Recognized BAli-Phy ancestral placeholders are removed before decoding.

Write the resulting FASTA alignment to standard output, or to the file named by `--out`.
`--out-probabilities` writes one support value per output column, in the same order, so that
less certain parts of the alignment can be identified. Diagnostics go to standard error.

# SCORING

Let p be a column's estimated probability and n its number of residues. The score combines
contributions from all columns in the proposed alignment. `--score` selects how they are combined:

**sum**
: Maximize the sum of p over columns.

**wsum**
: Maximize the sum of `n*p`, weighting each column by its residue count.

**wsum2**
: Maximize the sum of `n*(n-1)*p/2`, weighting each column by its number of residue pairs.
  This is the default. A column with only one residue contributes zero to this score.

**multiply**
: Maximize the product of column probabilities, computed as a sum of log probabilities.

`--column-pr homology` estimates a column's probability from how often the same residues and gaps
occur together. Residues are identified by their positions in their sequences, not by their letter
values. For a sequence with a gap in that column, the location of the gap within the sequence does
not matter. `--column-pr alignment` also requires that gap to occur at the same position between
residues. Both estimates are fractions of the retained sample, without added counts.

# OPTIONS

**-h**, **--help**
: Print help and exit.

**--alphabet** _ALPHABET_
: Specify the alphabet instead of inferring it from input. Examples include `DNA`, `RNA`,
  `Amino-Acids`, `Amino-Acids+stop`, `Triplets`, `Codons`, and `Codons+stop`.

**-s**, **--skip** _N_
: Skip the first N alignments. Default: 0.

**-m**, **--max-alignments** _N_
: Retain at most N alignments by thinning across the input, rather than stopping after the first N.
  Default: 1000. Use -1 to retain all alignments after skipping.

**--score** _NAME_
: Select `sum`, `wsum`, `wsum2`, or `multiply`. Default: `wsum2`. See SCORING.

**--column-pr** _TYPE_
: Select `homology` or `alignment` column probabilities. Default: `homology`.

**-S**, **--sort** _BOOL_
: Reorder columns that can move independently to group similar gaps. Takes an explicit Boolean
  value, such as `true` or `false`. Default: true. This preserves which residues occur together.

**-o**, **--out** _FILE_
: Write the decoded alignment to FILE. Default: `-`, meaning standard output.

**-p**, **--out-probabilities** _FILE_
: Write one probability per output alignment column, in output order, with no header. These are
  always calculated using the `homology` definition above, even when decoding uses
  `--column-pr alignment`.

**--debug-graph** _FILE:N_STATES_
: Write a Graphviz DOT description of the decoding graph to FILE, limiting the states shown to
  the beginning of its topological ordering. The argument must include both the filename and
  state limit, separated by a colon. The selected path is highlighted.

**-V**, **--verbose**
: Print additional diagnostics to standard error.

# EXAMPLES

Decode an alignment with the default residue-pair weighting:

    alignment-max --skip 100 < samples.fastas > decoded.fasta

Write the alignment and its column probabilities to separate files:

    alignment-max --out decoded.fasta --out-probabilities columns.txt < samples.fastas

Use the product objective and retain the decoder's column ordering:

    alignment-max --score multiply --sort false < samples.fastas > decoded.fasta

Write a graph for inspection with Graphviz:

    alignment-max --debug-graph decoding.dot:100 < samples.fastas > decoded.fasta

# SEE ALSO

**alignment-consensus**(1), **alignment-gild**(1), **alignment-distances**(1)

# REPORTING BUGS

BAli-Phy online help: <https://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
