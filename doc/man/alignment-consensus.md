% alignment-consensus(1)
% Benjamin Redelings
% October 2026

# NAME

**alignment-consensus** - Construct a consensus from sampled alignments.

# SYNOPSIS

**alignment-consensus** \[OPTIONS\] \[_SAMPLE_ ...\]

# DESCRIPTION

Construct one alignment summarizing the agreement among a sample of alternative alignments of
the same sequences, such as a sample produced by BAli-Phy. Use it to display which parts of an
alignment are well supported when the placement of gaps and residues is uncertain. A residue is
a sequence unit, such as a nucleotide or amino acid. Residues in the same alignment column are
proposed to correspond to the same ancestral position.

Support for a relationship is the fraction of sampled alignments that place particular residues
in the same column, or place a residue opposite a gap. For example, two residues aligned together
in 80 of 100 samples have support 0.8. This concerns their alignment, regardless of whether their
letters are identical. The support threshold controls which relationships are used to build the
consensus. The default is 0.75.

# CONSENSUS METHODS

Ordinary consensus (`--cutoff`) starts with separate residues and joins columns in decreasing
order of support, preserving sequence order and avoiding conflicting assignments. Joining columns
can imply additional relationships with lower support: placing A with B and B with C also places
A with C. Strict merging (`--strict`) checks the support of those implied relationships as well.
If both options are supplied, strict merging runs first, followed by ordinary merging.

Setting either threshold to 1 selects *strict consensus*: residues appear together only if they
occur together in every sampled alignment. This calculation also uses less memory than the
pairwise support calculation needed at other thresholds.

# INPUT AND OUTPUT

Each sample file contains FASTA alignments separated by empty lines. Filenames are positional;
with no filenames, read standard input. A filename of `-` also reads standard input. Sequence names
must match across samples, although their order may differ. For each sequence, its length after
removing gaps must be the same in every sample. Recognized BAli-Phy ancestral placeholders are removed;
`--chop-to` can explicitly select the first sequences before that removal.

Write the consensus as one FASTA alignment on standard output. Ordinary and strict merging can
leave `?` cells where neither a residue nor a gap assignment is established. Strict consensus
at threshold 1 writes `-` wherever a sequence has no residue in a retained group. Diagnostics go
to standard error.

# OPTIONS

**-h**, **--help**
: Print help and exit.

**--alphabet** _ALPHABET_
: Specify the alphabet instead of inferring it from input. Examples include `DNA`, `RNA`,
  `Amino-Acids`, `Amino-Acids+stop`, `Triplets`, `Codons`, and `Codons+stop`.

**-s**, **--skip** _N_
: Skip the first N alignments in each sample file. Default: 0.

**-m**, **--max** _N_
: Retain at most N alignments per file by thinning across the input, rather than stopping after
  the first N. Default: 1000. Use -1 to retain all alignments after skipping.

**--strict** _P_
: Require support P for relationships introduced by strict merging. A value of 1 selects
  strict consensus. See CONSENSUS METHODS.

**--cutoff** _P_
: Build an ordinary consensus using support threshold P. Defaults to 0.75 when neither threshold
  is specified. A value of 1 selects strict consensus. See CONSENSUS METHODS.

**--uncertainty** _FILE_
: Write uncertainty versus support to FILE, as described below. Incompatible with `--strict 1`
  or `--cutoff 1`.

**--chop-to** _N_
: Keep only the first N sequences, in the sequence order established by the first input alignment.

**-V**, **--verbose** \[_LEVEL_\]
: Print additional diagnostics. With no value, use level 1; larger values give more detail.

# UNCERTAINTY OUTPUT

The optional uncertainty file can be plotted to show how much of the alignment remains unresolved
as weaker relationships are accepted. It records progress through merging, down to the requested
threshold. If both stages run, only the ordinary stage's curve is written. There is no header.
Each row has three whitespace-separated numbers:

1. Support expressed as base-10 log odds: `log10((k+1)/(S-k+1))`, where k of the S retained
   alignments support the relationship. Larger values mean stronger support. The added counts
   keep the value finite.
2. Unresolved alignment cells divided by the total number of residues in all sequences.
3. Alignment columns divided by the mean ungapped sequence length.

An unresolved cell has neither a residue nor an established gap assignment for that sequence.
The second column can exceed 1; it is not a probability of error. Rows appear in increasing support
order and record the state reached at the corresponding support count during merging. There need
not be a row for every possible count. An empty curve is possible if no relationships are processed.

# EXAMPLES

Construct a consensus after discarding initial samples:

    alignment-consensus --skip 100 samples.fastas > consensus.fasta

Combine two samples and write an uncertainty curve:

    alignment-consensus --cutoff 0.75 --uncertainty uncertainty.txt run1.fastas run2.fastas > consensus.fasta

Retain only residue groups shared by every sampled alignment:

    alignment-consensus --strict 1 < samples.fastas > strict.fasta

# SEE ALSO

**alignment-max**(1), **alignment-gild**(1), **alignment-distances**(1)

# REPORTING BUGS

BAli-Phy online help: <https://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
