% joint-parsimony(1)
% Benjamin Redelings
% October 2026

# NAME

**joint-parsimony** - Report parsimony and indel summaries for joint alignment and tree samples.

# SYNOPSIS

**joint-parsimony** _ALIGNMENTS_ _TREES_ \[OPTIONS\]

# DESCRIPTION

**joint-parsimony** reports a minimum substitution count and summaries of insertion/deletion
runs for each sampled alignment and tree, describing variation across samples. Results go
to standard output; diagnostics go to standard error.

_ALIGNMENTS_ and _TREES_ are required positional filenames containing alignment samples and
corresponding Newick tree samples. Multiple FASTA alignments can be separated by blank lines;
the tree file contains one tree per line. Each retained tree must correspond to the alignment
paired with it.

**--subsample** thins only the tree file, starting with its first tree. The default is 10,
for alignments saved once per ten tree samples. Use **--subsample=1** when the two files already
have matching sampling frequencies. If counts differ, the tool warns and uses only the pairs
available from the beginning of both files.

# OUTPUT

Each alignment/tree pair produces one line containing the following labeled quantities:

`length`
: The minimum number of state changes needed to explain the leaf observations on the tree,
  summed across alignment columns. Changes between different alphabet states have unit cost;
  with a codon alphabet, a change between two codons counts as one, even if several nucleotides
  differ. This is a parsimony score, not a count of differences in the supplied ancestral sequences.

`#indels`
: Number of contiguous insertion/deletion runs across all branches. A run consists of gaps
  in one endpoint sequence opposite symbols in the other; switching the gapped side starts
  a new run. These runs do not necessarily correspond one-to-one to historical indel events.

`|indels|`
: Total length of those runs in alphabet symbols. A codon counts as one symbol.

`phase0`, `phase1`, `phase2`, `no-phase`
: Counts of indel runs classified by the ungapped sequence positions before each run,
  measured in nucleotides modulo three. If both endpoint sequences have the same phase,
  the corresponding phase count is incremented; otherwise `no-phase` is incremented.
  These fields appear only for DNA/RNA and nucleotide triplet/codon alphabets.

Reading-frame interpretation assumes coding sequences beginning at a codon boundary.
Whole-codon gaps occur at phase zero. The indel counts use the gap patterns on tree branches;
the parsimony score instead minimizes state changes using the leaf observations.

# EXAMPLES

Summarize files containing one tree for each alignment:

    joint-parsimony samples.fastas samples.trees --subsample=1

Treat aligned nucleotide triplets as codons:

    joint-parsimony samples.fastas samples.trees \
        --subsample=1 --alphabet=Codons

# OPTIONS

**`--subsample`** _NUM_
: Read every NUMth tree, starting with the first. The default is 10. Alignment samples are not thinned.

**`--alphabet`** _NAME_
: Select the alignment alphabet, for example `DNA`, `RNA`, `Amino-Acids`, or `Codons`.
  By default, infer the alphabet from the input.

**`-h`**, **`--help`**
: Display help and exit.

# SEE ALSO

**joint-indels**(1)

# REPORTING BUGS

BAli-Phy online help: <https://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
