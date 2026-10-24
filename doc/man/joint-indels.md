% joint-indels(1)
% Benjamin Redelings
% October 2026

# NAME

**joint-indels** - Summarize indels on a branch or extract sampled ancestral sequences.

# SYNOPSIS

**joint-indels** _ALIGNMENTS_ _TREES_ **--partition** _TAXA_ \[OPTIONS\]

**joint-indels** _ALIGNMENTS_ _TREES_ **--extract-sequences** _QUERY_ \[OPTIONS\]

# DESCRIPTION

**joint-indels** examines insertion and deletion events on a selected branch across joint
alignment and tree samples. It can report event counts or the locations and sequences of
individual events. Alternatively, it extracts sampled sequences for named ancestral nodes
in a query tree. Results go to standard output; diagnostics go to standard error.

_ALIGNMENTS_ and _TREES_ are required positional filenames containing alignment samples and
corresponding Newick tree samples. Multiple FASTA alignments can be separated by blank lines;
the tree file contains one tree per line. The files must describe the same taxa, and each
retained tree must correspond to the alignment paired with it.

**--subsample** thins the tree file only, starting with its first tree. The default is 10,
for alignments saved once per ten tree samples. Use **--subsample=1** when both files already
contain corresponding samples at the same frequency. If the counts differ, the tool warns
and uses only the pairs available from the beginning of both files.

# OUTPUT

## Indels on a branch

**--partition** names all taxa on one side of the selected branch, separated by colons.
A single taxon selects its terminal branch. Sequence 1 is the sequence at the branch endpoint
on the named side; sequence 2 is at the other endpoint. An indel event is a contiguous run of
gaps in one sequence opposite symbols in the other.

Ordinary output is a tab-separated table with columns `Len` and `Indels`, one row per sample.
`Len` is the pairwise alignment length, excluding columns where both sequences have gaps.
`Indels` counts events. A sample without the selected branch produces `NA` and `0`; this
marks an unavailable comparison, not an observed absence of indels on that branch.

With **--details**, the columns are:

`Sample`
: Zero-based index of the retained alignment/tree pair, not an MCMC iteration number.

`Start1`, `Start2`, `End1`, `End2`
: Ungapped sequence counts before and through the end of the event. The difference between
  End and Start is the event length in the sequence receiving symbols.

`Type`
: `I` when sequence 2 has symbols opposite a gap in sequence 1; `D` for the reverse.
  These describe the two sides of the branch, not a reconstruction of evolutionary direction.

`Length`, `Len1`, `Len2`
: Event length and total ungapped lengths of the two sequences, respectively.

`Sequence`
: The inserted or deleted sequence, from sequence 2 for `I` and sequence 1 for `D`.

All lengths and coordinates count alphabet symbols: a codon counts as one symbol, although
its complete nucleotide spelling appears in `Sequence`. Each event produces one row. A
present branch with no indels produces no detail rows. A missing branch produces a short
row containing only the sample index and `NA`.

## Ancestral sequence extraction

With **--extract-sequences**, the first tree in _QUERY_ identifies internal nodes to extract.
It must have the same taxa as the sampled trees. Only named internal query nodes are printed.
Matching depends on the taxon splits around a node, not on internal labels or branch lengths:
the corresponding branches must meet at one sampled node. Topology elsewhere may differ.
An unresolved query node does not match several sampled nodes that jointly resolve it.

Each sample produces FASTA records for its matched nodes, using the query node names and
retaining alignment gaps. Samples are separated by blank lines; unmatched nodes are omitted.
The final diagnostics report each named node's matching frequency. Extraction takes precedence
if **--partition** or **--details** is also supplied.

Sequence extraction does not reliably handle degree-two nodes (nodes connected to exactly
two branches), including degree-two roots. Such trees may cause matching internal nodes to
be reported as absent.

# EXAMPLES

Report individual events on the branch separating A and B from the other taxa:

    joint-indels samples.fastas samples.trees \
        --subsample=1 --partition=A:B --details

Extract sequences for named internal nodes in a query tree:

    joint-indels samples.fastas samples.trees \
        --subsample=1 --extract-sequences=query.tree

# OPTIONS

**`--partition`** _TAXA_
: Select a branch by a colon-separated list of taxa on one side. Required unless extracting sequences.

**`--details`**
: Report individual events instead of counts.

**`--extract-sequences`** _QUERY_
: Extract sampled sequences for named internal nodes in the query tree.

**`--subsample`** _NUM_
: Read every NUMth tree, starting with the first. The default is 10. Alignment samples are not thinned.

**`--alphabet`** _NAME_
: Select the alignment alphabet, for example `DNA`, `RNA`, `Amino-Acids`, or `Codons`.
  By default, infer the alphabet from the input.

**`-V`**, **`--verbose`**
: Show more diagnostics. Repeat the flag or use **--verbose=2** for more detail.

**`-h`**, **`--help`**
: Display help and exit.

# SEE ALSO

**joint-parsimony**(1), **summarize-ancestors**(1)

# REPORTING BUGS

BAli-Phy online help: <https://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
