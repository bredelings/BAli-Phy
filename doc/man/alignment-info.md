% alignment-info(1)
% Benjamin Redelings
% Feb 2018

# NAME

**alignment-info** - Show useful statistics about the alignment.

# SYNOPSIS

**alignment-info** _alignment-file_ [_tree-file_] \[OPTIONS\]

# DESCRIPTION

Show useful statistics about the alignment.

# ALLOWED OPTIONS:
**-h**, **--help**
: produce help message

**--align** _arg_
: file with sequences and initial alignment

**--tree** _arg_
: file with initial tree

**--alphabet** _arg_
: specify the alphabet: DNA, RNA, Amino-Acids, Triplets, or Codons

**`--site-parsimony`**
: Print a per-column TSV table and exit before the ordinary statistics, including indel
  analyses. Requires a tree and exactly one alignment sequence per tree tip, matched by name.
  Cannot be combined with **`--show-names`**, **`--show-lengths`**, or **`--erase-empty-columns`**.

**-e**, **--erase-empty-columns**
: Remove columns with no characters (all gaps).

**-N**, **--show-names**
: Print the sequence-names and exit

**-L**, **--show-lengths**
: Print the sequence-lengths and exit


# PER-SITE PARSIMONY

With **`--site-parsimony`**, standard output contains only a tab-separated table with columns
`column`, `n_called`, `n_states`, and `parsimony`. Errors go to standard error.

Every input alignment column is retained, including empty and constant columns. `column` is
its original 1-based index, not a reference-genome coordinate. For multicharacter alphabets,
positions count alphabet units rather than individual nucleotides.

`n_called` counts tips with an exact alphabet state; `n_states` counts distinct exact states.
Gaps and ambiguous calls do not contribute to these counts. The parsimony score uses the
existing unit-cost calculation: equal states cost zero and unequal states cost one, ignoring
branch lengths. Partial ambiguities still constrain scoring to their allowed states; gaps
and fully unknown observations are unconstrained. Thus exact-state counts alone do not
summarize all constraints when partial ambiguities occur.

For a biallelic site with otherwise fully missing calls, score 1 means that a tree edge
separates the two state groups; a larger score requires multiple changes. This does not
identify the biological cause of conflict. Empty columns receive score zero.

# EXAMPLES

Score one alignment against one tree:

```bash
alignment-info alignment.fasta tree.newick --site-parsimony > sites.tsv
```

Compare one mitochondrial alignment against two trees in separate runs:

```bash
alignment-info mitochondrial.fasta mitochondrial.tree --site-parsimony > mitochondrial.tsv
alignment-info mitochondrial.fasta concatenated.tree --site-parsimony > concatenated.tsv
```

To check a sample's contribution, prepare an external copy with that sample's calls replaced
by `?`, retaining its row and every column. Score both alignments on the same tree:

```bash
alignment-info original.fasta tree.newick --site-parsimony > original.tsv
alignment-info masked.fasta tree.newick --site-parsimony > masked.tsv
```

Compare tables by column outside this tool. Matching sample sets and column coordinates
across runs are the caller's responsibility. This option does not mask samples, prune trees,
compare runs, or process coverage information.

# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

