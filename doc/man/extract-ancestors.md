% extract-ancestors(1)
% Benjamin Redelings
% October 2026

# NAME

**extract-ancestors** - Extract named ancestral sequences from sampled alignments and trees.

# SYNOPSIS

**extract-ancestors** \[OPTIONS\] **-A** _ALIGNMENTS_ **-T** _TREES_ \[…\]

# DESCRIPTION

Extract sampled sequences for ancestors identified by their relationships to the observed species.
For each sampled alignment and tree, write the leaf sequences and the requested ancestral
sequences that are present in that tree. This gives a sample of ancestral sequences while
preserving variation in the alignment and tree topology.

Use **--nodes** or **--groups** to name the ancestors of interest. An ancestor may be absent
from some sampled trees; its row is omitted from those output alignments. To produce one summary
alignment instead of a sample of alignments, use **summarize-ancestors**.

# INPUT

Supply alignment samples with **-A** and corresponding tree samples with **-T**. Alignment files
contain FASTA alignments separated by empty lines; tree files contain one Newick tree per line.
Repeat both options for multiple chains: the first alignment file pairs with the first tree file,
the second with the second, and so on.

Supply sampled sequences for all tree nodes to extract ancestral letters. Sequence names must
match the corresponding tree labels. Leaf-only alignments are accepted, but do not provide
sampled ancestral letters. All chains must describe the same leaf sequences.

The files in each pair must start at corresponding samples. With the default **--subsample 10**,
alignment records 0, 1, 2, … pair with tree records 0, 10, 20, …, as in BAli-Phy logs that save
alignments every ten tree records. Use **--subsample 1** when each alignment already has one
corresponding tree. This option matches logging frequencies; it does not thin the alignment file.

# ANCESTOR QUERIES

An ancestor is identified by its relationship to the named leaves (the observed sequences),
not by a node number that might change between sampled trees. Queries do not require a fixed root. Query trees must have
the same leaf names as the sampled trees.

**--nodes** reads a Newick tree whose labelled internal nodes name the requested ancestors.
For example, save the following as `query.tree`:

    (a,b,(c,d)CD)AB;

This requests ancestors `AB` and `CD`. A query node matches only when all the branches meeting
at that node describe the same divisions of leaves around one node in the sampled tree.
A node with more than three incident branches must match that branching pattern; it does not
match a tree that resolves those branches through several separate nodes.

**--groups** reads named groups, one per line. For example, `groups.txt` could contain:

    CD = c d

This requests the endpoint on the `c d` side of a branch separating those leaves from all other
leaves. Only this division of leaves must match; the other branches are unconstrained.
The spaces around `=` are required. Empty lines and lines beginning with `#` are ignored.

A groups file may also contain Newick trees, one per line. For each edge with labelled endpoints,
the program requests its internal endpoints, using names such as `CD<=AB`: this means the endpoint
`CD`, approached along the edge from `AB`. Both internal endpoints of an edge can be requested.
Node and group queries may be used together.

# OUTPUT

Write FASTA alignments separated by empty lines to standard output. Each contains the leaf rows
followed by the matching ancestors under their query names. Original ancestral rows not selected
by a query are omitted. Samples from successive input chains are written in input-file order.

Standard error reports how often each queried ancestor occurs in the sample, as well as any
diagnostics. Occurrence measures the frequency of the relevant tree topology, not certainty
about individual ancestral letters.

# OPTIONS

**-h**, **--help**
: Print help and exit.

**-A**, **--alignments** _FILE_
: Alignment sample file. Required; repeat for each chain.

**-T**, **--trees** _FILE_
: Corresponding tree sample file. Required; repeat in alignment-file order.

**--alphabet** _ALPHABET_
: Specify the sequence alphabet instead of inferring it from input, for example `DNA` or `Codons`.

**-x**, **--subsample** _N_
: Tree-record stride used to pair trees with alignments. Default: 10.

**-n**, **--nodes** _FILE_
: Newick tree with labelled internal nodes defining ancestor queries.

**-g**, **--groups** _FILE_
: Named leaf groups or labelled Newick trees defining branch-endpoint queries.

**-V**, **--verbose** \[_LEVEL_\]
: Set diagnostic verbosity on standard error. Supplying the option without a value means level 1.

# EXAMPLES

Extract ancestors labelled in a query tree from BAli-Phy logs:

    extract-ancestors -A C1.P1.fastas -T C1.trees --nodes query.tree > ancestors.fastas

Use a named-group query with files containing one tree per alignment:

    extract-ancestors -A paired.fastas -T paired.trees --subsample 1 \
      --groups groups.txt > ancestors.fastas

# SEE ALSO

**summarize-ancestors**(1), **alignment-consensus**(1)

# REPORTING BUGS

BAli-Phy online help: <https://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
