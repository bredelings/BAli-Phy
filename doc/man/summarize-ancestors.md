% summarize-ancestors(1)
% Benjamin Redelings
% October 2026

# NAME

**summarize-ancestors** - Add estimated ancestral sequences to a template alignment.

# SYNOPSIS

**summarize-ancestors** \[OPTIONS\] _TEMPLATE_ **-A** _ALIGNMENTS_ **-T** _TREES_ \[…\]

# DESCRIPTION

Estimate ancestral sequences from sampled alignments and trees, and display them alongside the
observed sequences in a chosen template alignment. The template supplies the leaf alignment;
the samples supply ancestral letters and their alignment relationships to the leaves.

Use **--nodes** or **--groups** to identify ancestors by their relationships to named species.
The program adds a summary row for each ancestor occurring in a sufficient fraction of the
retained trees. This produces one alignment for inspecting ancestral sequences alongside an
alignment estimate. To retain the individual sampled ancestral sequences instead, use
**extract-ancestors**.

# INPUT

TEMPLATE is a required positional filename containing a leaf alignment. It must contain the
same leaf names and ungapped sequences as the samples. Its columns are preserved in the output.

Supply alignment samples with **-A** and corresponding tree samples with **-T**. Alignment files
contain FASTA alignments separated by empty lines; tree files contain one Newick tree per line.
Repeat both options for multiple chains: the first alignment file pairs with the first tree file,
the second with the second, and so on. All chains must describe the same leaf sequences.

Supply sampled sequences for all tree nodes to estimate ancestral letters. Sequence names must
match the corresponding tree labels. Leaf-only alignments are accepted, but do not provide
sampled ancestral letters.

# SAMPLE SELECTION

The files in each pair must start at corresponding samples. With the default **--subsample 10**,
alignment records 0, 1, 2, … pair with tree records 0, 10, 20, …, as in BAli-Phy logs that save
alignments every ten tree records. Use **--subsample 1** for files containing one tree per alignment.
This stride matches logging frequencies; **--thin** selects among the resulting pairs.

Within each chain, **--skip** and **--until** select an inclusive range of tree-record positions,
counting from zero in the supplied tree file. In original logs with one tree per iteration, these
positions correspond to iteration numbers. For filtered files, they refer to positions in those
files. Then **--thin** retains every kth eligible pair, starting with the first eligible pair.

**--max** limits the total retained sample across all chains by further thinning within chains.
The final allocation is proportional to the counts remaining in each chain, rather than giving
every chain equal weight. Samples are spread through the chains rather than taking only their
first records.

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

# CALCULATION

For each template column and each sampled alignment, choose a leaf character at random from that
column. Locate the same character in the sampled alignment and record the ancestral symbol in
its column, if the queried ancestor is present in that tree. This uses sequence positions to
connect columns across different alignments. Because the choice is random, repeated runs can
produce different summaries.

For each ancestor and template column, output a gap if at least half the recorded observations
are gaps. Otherwise output the most frequent non-gap symbol. Ambiguity symbols are counted as
observed symbols. A column with no observations receives an unknown symbol.

The inclusion thresholds **--nodes-min** and **--groups-min** refer to the fraction of retained
trees containing the queried ancestor. They do not measure support for its individual letters;
those are summarized from the samples in which the ancestor occurs.

# OUTPUT

Write one FASTA alignment to standard output: the template's leaf rows followed by the included
ancestral rows, named by their queries. Standard error reports the occurrence frequency of each
queried ancestor and any diagnostics.

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

**--skip** _N_
: First tree-record position to retain in each chain, inclusive and zero-based. Default: 0.

**--until** _N_
: Last tree-record position to retain in each chain, inclusive and zero-based. Default: no limit.

**--thin** _K_
: Keep every kth eligible alignment/tree pair independently in each chain. Default: 1.

**-m**, **--max** _N_
: Retain at most N samples in total across all chains by thinning. Default: 500.

**-n**, **--nodes** _FILE_
: Newick tree with labelled internal nodes defining ancestor queries.

**-g**, **--groups** _FILE_
: Named leaf groups or labelled Newick trees defining branch-endpoint queries.

**--nodes-min** _FRACTION_
: Minimum fraction of retained samples containing a queried node for its row to be included.
  Default: 0.34.

**--groups-min** _FRACTION_
: Minimum fraction of retained samples containing a queried branch endpoint for its row to be
  included. Default: 0.34.

**-V**, **--verbose** \[_LEVEL_\]
: Set diagnostic verbosity on standard error. Supplying the option without a value means level 1.

# EXAMPLES

Add ancestral estimates to a template alignment using BAli-Phy logs:

    summarize-ancestors summary.fasta -A C1.P1.fastas -T C1.trees \
      --nodes query.tree > ancestral-summary.fasta

Combine two chains after discarding the first 1000 tree records from each, retaining at most
500 samples in total:

    summarize-ancestors summary.fasta \
      -A run1/C1.P1.fastas -T run1/C1.trees \
      -A run2/C1.P1.fastas -T run2/C1.trees \
      --skip 1000 --max 500 --groups groups.txt > ancestral-summary.fasta

# SEE ALSO

**extract-ancestors**(1), **alignment-consensus**(1), **alignment-draw**(1)

# REPORTING BUGS

BAli-Phy online help: <https://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
