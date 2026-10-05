% alignment-chop-internal(1)
% Benjamin Redelings
% October 2026

# NAME

**alignment-chop-internal** - Remove ancestral sequences from FASTA alignment samples.

# SYNOPSIS

**alignment-chop-internal** **-N** _count_ < _alignments-file_ > _leaves.fastas_

**alignment-chop-internal** **`--tree`** _tree-file_ < _alignments-file_ > _leaves.fastas_

# DESCRIPTION

Read FASTA alignments from standard input and write selected sequences from each
alignment to standard output. Read input through shell redirection or a pipe;
there is no positional input filename.

Choose exactly one selection method: retain the first N sequences with **`--nleaves`**,
or retain sequences whose names occur as leaves in **`--tree`**. Ancestral sequences are not identified from their contents.

Each input alignment begins with a FASTA header whose first character is **>** and
ends at an empty line or end of input. Separate successive alignments with empty
lines. Text before or between alignment blocks is skipped while looking for the next
header. Output alignments are in FASTA format, separated by empty lines.

Sequence names are the text after **>** up to the first space or tab; the remaining
header text is a comment. Letters are converted to uppercase, but sequence names
retain their case. Retained sequences keep their input order and columns, including
columns containing only gaps. The tool does not interpret a sequence alphabet or
require equal sequence lengths.

# OPTIONS

**-h**, **`--help`**
: Print usage information and exit.

**-N** _count_, **`--nleaves`** _count_
: Keep the first _count_ sequences in each alignment. This assumes the desired leaf
  sequences precede ancestral sequences. A count of zero produces no sequences.
  An alignment with fewer than _count_ sequences causes an error.
  Cannot be combined with **`--tree`**.

**`--tree`** _tree-file_
: Read the first tree from a Newick or NEXUS file and keep sequences whose names
  match its leaf labels. Matching is case-sensitive. Internal-node labels and
  branch lengths do not affect selection. Output follows alignment order, not tree
  order. Cannot be combined with **`--nleaves`**.

  Empty leaf labels are ignored with a warning; duplicate nonempty leaf labels
  cause an error. An alignment with fewer sequences than the number of distinct
  nonempty leaf labels also causes an error. Otherwise, selection keeps every
  sequence whose name matches a leaf label: it does not check that every leaf
  occurs in the alignment or that alignment sequence names are unique.

# EXAMPLES

Keep the first five sequences in every sampled alignment:

```
alignment-chop-internal -N 5 < samples.fastas > leaves.fastas
```

Keep sequences named by the leaves of a reference tree:

```
alignment-chop-internal --tree reference.tree < samples.fastas > leaves.fastas
```

Select the last alignment, then retain its leaf sequences:

```
alignment-find < samples.fastas | alignment-chop-internal --tree reference.tree > leaves.fasta
```

# EXIT STATUS

Returns 0 on success (including **`--help`**) and 1 when an error is reported.
Warnings and error messages are written to standard error. Input containing no
alignment blocks produces no output and succeeds, provided the selection options
and any tree input are valid. If a later alignment fails, output from earlier
alignments may already have been written.

# SEE ALSO

**alignment-find**(1), **alignment-cat**(1), **cut-range**(1)

# REPORTING BUGS

BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
