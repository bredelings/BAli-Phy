% alignment-gild(1)
% Benjamin Redelings
% October 2026

# NAME

**alignment-gild** - Estimate support for each residue's placement in an alignment.

# SYNOPSIS

**alignment-gild** \[OPTIONS\] _ALIGNMENT_ _TREE_ \[_SAMPLE_ ...\]

# DESCRIPTION

Estimate the probability that each residue in a reference alignment should align to a hypothetical
root character in its column. A residue is a sequence unit, such as a nucleotide or amino acid;
the hypothetical root character represents a common ancestral position for the column. The estimates use a sample of
alternative alignments of the same sequences and a tree describing their evolutionary relationships.

Use these annotations to see which residues have well-supported alignment positions and which
have uncertain placements. For example, most residues in a column may align together consistently
across the sample, while one sequence's residue often aligns elsewhere. The annotations describe
that difference within the column. A separate probability summarizes support for the entire column.

The hypothetical ancestral position is chosen separately for each column, using the tree's
branching pattern and the sampled alignment relationships. It is not fixed to the supplied tree's
root. The probabilities are estimates based on this tree approximation. The program writes an
annotation table; it leaves the reference alignment itself unchanged.

# INPUT

ALIGNMENT and TREE are required positional filenames. ALIGNMENT contains one reference alignment;
TREE contains a Newick or NEXUS tree. SAMPLE files contain FASTA alignments separated by empty
lines. With no sample filenames, read the sample from standard input. A sample filename of `-`
also reads standard input.

The reference sequence names must match the tree leaves, and each leaf sequence must have the
same length after gaps are removed in the reference and all samples. Sequence order may differ.
Recognized BAli-Phy ancestral placeholders are removed from the reference. Each sample file may
contain either leaf sequences alone or sequences for every tree node; different files may use
different layouts. Only leaf sequences contribute to the annotations.

For samples containing ancestral sequences, their names must match the internal-node labels in
the tree. Unnamed internal nodes are assigned `A` followed by their node index. The same sequence
names must be present throughout each sample file.

The tree must be fully resolved: after removing nodes with only two neighbors, each internal node
must have three neighbors. Its branching pattern is used, but its supplied root and branch lengths
do not determine the annotation probabilities.

# OUTPUT

Write a space-separated table to standard output, with one row per reference column after
empty-column removal. Diagnostics go to standard error. The first line lists leaf sequence names
in tree order. Each data row contains:

- One estimated probability for each sequence, in header order. These values are also called
  *weights*. A residue's weight describes support for its alignment to the hypothetical root
  character. Gaps and unknown cells are assigned values as described under CALCULATION.
- A final probability for the whole column. This additional field has no name in the header.

The whole-column probability is `(k+0.5)/(S+1)`, where S is the number of retained sample alignments
and k is the number containing the reference column's residues together with its specified gaps.
Unknown reference cells impose no constraint. The added counts keep the estimate away from zero
and one, even when all samples agree.

# CALCULATION

For each reference column, count how often each pair of its residues aligns together in the sample,
using their positions in their sequences to identify them. Smooth these pairwise probabilities
with added counts. For columns with three or more residues, convert the probabilities to distances
using negative logarithms and fit branch lengths on the tree by least squares. Negative fitted
lengths are replaced with zero. Choose the internal node maximizing the sum of `exp(-distance)`
to the residues as the hypothetical root. Each residue's weight is `exp(-distance)` from that node.

A single residue receives weight 1. With two residues, both receive the square root of their
pairwise support. A gap receives the largest product of a residue's weight and the pairwise support
for that gap opposite the residue. An unknown cell is assigned weight 1 without assessing its
alignment. These per-sequence weights and the whole-column probability measure different things.

# OPTIONS

**-h**, **--help**
: Print help and exit.

**--alphabet** _ALPHABET_
: Specify the alphabet instead of inferring it from the reference. Examples include `DNA`, `RNA`,
  `Amino-Acids`, `Amino-Acids+stop`, `Triplets`, `Codons`, and `Codons+stop`.

**--skip** _N_
: Skip the first N alignments in each sample file. Default: 0.

**--max-alignments** _N_
: Retain at most N alignments per file by thinning across the input, rather than stopping after
  the first N. Default: 1000. Use -1 to retain all alignments after skipping.

**-V**, **--verbose**
: Print additional diagnostics to standard error.

# EXAMPLES

Annotate a reference using one sample file:

    alignment-gild reference.fasta tree.nwk samples.fastas > annotations.txt

Combine samples after discarding initial alignments from each file:

    alignment-gild --skip 100 reference.fasta tree.nwk run1.fastas run2.fastas > annotations.txt

Read samples from standard input:

    alignment-gild reference.fasta tree.nwk < samples.fastas > annotations.txt

# SEE ALSO

**alignment-consensus**(1), **alignment-max**(1), **alignment-draw**(1)

# REPORTING BUGS

BAli-Phy online help: <https://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
