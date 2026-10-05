% alignment-info(1)
% Benjamin Redelings
% September 2026

# NAME

**alignment-info** - Report alignment statistics or per-column parsimony scores.

# SYNOPSIS

**alignment-info** [_alignment-file_] [_tree-file_] \[OPTIONS\]

# DESCRIPTION

By default, print a human-readable summary of alignment dimensions, variation, mean mismatch
fractions, gaps, and state frequencies. Supplying a tree adds minimum-substitution totals.

Two alternative modes return before the ordinary report:

- **`--show-names`** and **`--show-lengths`** list information about the input sequences.
- **`--site-parsimony`** writes a per-column TSV table for one alignment and one tree.

Reports go to standard output; errors go to standard error. The command does not infer a
tree or optimize branch lengths.

# INPUT AND PREPROCESSING

The first positional argument is the alignment file. If omitted, or given as `-`, it is read
from standard input. Use `-` when supplying a tree file as the second positional argument
and reading the alignment from standard input. The tree is optional for the ordinary report
and required for per-site parsimony. Branch lengths are not used for the reported parsimony
scores. Specify **`--alphabet`** when the alphabet cannot be inferred reliably.

For an ordinary report with a tree, sequence names are matched to tree-node names and rows
may be reordered. Normally supply one sequence per tip. The ordinary linking path can also
accept sequences for every tree node, then discard internal-node sequences; this path may
remove columns that become empty. Other mismatches are errors. Per-site mode instead requires
exactly one row per tip and preserves all columns.

For tip-only alignments, empty columns are retained by default. **`--erase-empty-columns`**
removes columns containing only gaps (`-`), unknown calls (`?`), or a mixture of the two.
DNA `N` denotes an unknown but present nucleotide and does not by itself make a column empty.
See LIMITATIONS for ordinary-report behavior on columns with no exact states.

An encoded alignment column contains one alphabet unit. With Triplets or Codons, one unit
represents three nucleotides. In ordinary reports, sequence lengths count present alphabet
units, including partial ambiguities and fully ambiguous non-gap states, but excluding gaps
and `?`. This differs from the raw-character lengths printed by **`--show-lengths`**.

# OPTIONS

**-h**, **`--help`**
: Print usage and options, then exit.

**`--alphabet`** _name_
: Specify the alphabet instead of inferring it, for example `DNA`, `RNA`, `Amino-Acids`,
  `Triplets`, or `Codons`.

**-e**, **`--erase-empty-columns`**
: Remove empty columns before the ordinary report. Cannot be used with **`--site-parsimony`**.

**-N**, **`--show-names`**
: Print one sequence name per line and exit. Combined with **`--show-lengths`**, print
  `name,length` per line. These listings have no header.

**-L**, **`--show-lengths`**
: Print one sequence length per line and exit. Length is the number of input characters other
  than `-` and `?`, before alphabet encoding: it counts nucleotides rather than codons even
  when **`--alphabet Codons`** is supplied. Names/lengths mode preserves input order and runs
  before tree loading, alphabet encoding, or empty-column removal. It does not validate a
  supplied tree or apply those preprocessing options.

**`--site-parsimony`**
: Print the TSV table described below and exit before ordinary statistics. Requires a tree
  and exactly one alignment sequence per tree tip, matched by name. Cannot be combined with
  **`--show-names`**, **`--show-lengths`**, or **`--erase-empty-columns`**.

**`--state-groups`**
: With **`--site-parsimony`**, append exact state counts and observed-tip group sizes from
  one optimal ancestral reconstruction. Requires **`--site-parsimony`**.

# ORDINARY REPORT

## Dimensions and lengths

The first lines report alignment columns, sequence count, alphabet, and the minimum, maximum,
mean, and median sequence lengths after preprocessing. Lengths are in alphabet units.

## Variation without indels

Under `w/o indels`, `non-const.` counts columns containing at least two different exact
alphabet states; `const.` is the remaining column count. `inform.` counts columns in which
at least two exact states each occur at least twice. Gaps and ambiguous observations do not
contribute to exact-state counts. Percentages use the total processed column count.

## Variation with indels

Under `w/ indels`, a column is nonconstant if it is nonconstant by the preceding rule or
contains any gap. It is informative if it is informative by the preceding rule or contains
at least two gaps and at least two present characters. Present characters include partial
ambiguities and fully ambiguous non-gap states; `?` contributes to neither side of this
presence/absence count. These are column classifications, not inferred indel events.

## Mean mismatch fraction

Each variation section reports an overall mismatch fraction between 0 and 1, replacing the
former minimum pairwise identity. It sums mismatching pair-column observations over the
alignment and divides by the total eligible pair-column observations. Sequence pairs are
therefore weighted by their numbers of comparable positions. With complete data, this equals
the ordinary mean pairwise mismatch fraction; with missing data, it need not equal a mean
that gives every sequence pair equal weight.

Without indels, only pairs of exact alphabet states are eligible: equal states match and
unequal states mismatch. With indels, an exact state opposite a gap also contributes one
comparison and one mismatch. Gap-gap pairs are excluded in both cases. Pairs involving `?`,
fully ambiguous calls such as DNA `N`, or partial ambiguities are excluded. This differs
from the former identity statistic, which compared ambiguous symbols literally.

At each column, let n be the number of exact calls, n_b the count of state b, and g the gap
count. There are n(n-1)/2 exact-state pairs, of which sum_b n_b(n_b-1)/2 match. The indel-inclusive
calculation adds ng to both the comparison and mismatch counts. Counts are summed over
columns before division. A result with no eligible comparisons is printed as `NA`, not zero.

## Gaps and estimated indel groups

The gap section reports the fraction of columns containing at least one gap and the fraction
of all alignment cells that are gaps. Unknown calls are not counted as gaps.

To estimate indel groups, the program finds contiguous gap runs in each sequence and groups
runs having identical start columns and lengths. For each group it compares the number of
sequences containing that exact run with the number having no gaps anywhere in that interval.
If the latter is smaller, it labels the group an insertion and uses that smaller count;
otherwise it labels it a deletion. Groups with a resulting count of zero are discarded.

The report contains:

- `indel groups`: the number of retained distinct runs;
- `separate`: the sum of the resulting counts over groups;
- `unique`: groups whose resulting count is one;
- `inform.`: groups whose count exceeds one and leaves more than one other sequence;
- `ins./del.`: numbers of groups assigned each label;
- gap-length range, mean, and median: one length per retained group, without weighting by
  its count, in alignment-column units.

The latter details are printed only when the total count is nonzero. These are alignment-based
heuristics, not a phylogenetic reconstruction of insertion/deletion history; supplying a tree
does not change how these groups are estimated.

## Tree lengths

When a tree is supplied, `tree length` is the minimum total number of state changes under
unit substitution costs, summed over columns. It is not the sum of input branch lengths.
Partial ambiguities constrain allowed states; gaps and fully unknown calls are unconstrained.

Triplet alphabets also report `tree length (nuc)`, using the number of nucleotide differences
between triplets as the change cost. Codon alphabets additionally report `tree length (aa)`,
with cost zero between codons encoding the same amino acid and one otherwise.

## Stop codons

For nucleotide alphabets, `Stop Codons` reports three slash-separated counts of literal
`TAA`, `TGA`, and `TAG` occurrences across sequences, grouped by their zero-based start offset
modulo three. The search uses stored alignment strings, including gaps, rather than ungapped
sequences. It is not a gene-aware or strand-aware translation, and it does not substitute
RNA spellings containing `U` for the searched `T` motifs.

## Frequencies, classes, and wildcards

`Frequencies` uses exact-state counts only, then adds a pseudocount of floor(sequence count / 2)
to every alphabet state before normalizing. These are smoothed frequencies, not raw observed
proportions; a state absent from the alignment can have a nonzero reported frequency.

`Classes` counts encoded partial ambiguities. `Wildcards` counts fully ambiguous non-gap
states, such as DNA `N`. Their percentages use the sum of exact-state, class, and wildcard
counts as the denominator, excluding gaps and `?`.

# LIMITATIONS AND COST

The ordinary report currently assumes that each retained column contains at least one exact
alphabet state. An assertion-enabled build stops on columns containing only gaps or ambiguous
observations. Removing empty columns does not remove all ambiguity-only columns. Per-site
mode supports these columns and retains their coordinates.

Mean mismatch fractions are computed from column counts without visiting sequence pairs.
For a fixed alphabet this takes time linear in sequence count times column count. Indel-group
estimation still performs additional scans and can contribute to runtime on large alignments.
Names/lengths mode and per-site mode bypass these ordinary-report calculations.

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

## State groups

Adding **`--state-groups`** appends `state_counts` and `state_groups`. For example,
`A:51;T:100` and `A:48,3;T:100` describe 51 observed A calls in two same-state components
and 100 T calls in one component. States appear in alphabet order and component sizes in
largest-first order. A field is `.` when there are no exact observed calls.

Groups are connected components of one minimum-cost ancestral reconstruction after cutting
all edges where the state changes. Only exact observed tip states contribute to component
sizes. Missing and ambiguous tips contribute zero, and components without counted tips are
omitted. Group sizes for a state sum to its exact observed count. Missing tips alone do not
split a component.

At the computational root, the first minimum-cost state in alphabet order is selected. Each
child then takes the first minimum-cost state conditional on its selected parent state.
This is a jointly valid reconstruction, but other equally optimal reconstructions may yield
different groups. Results can depend on the computational root and tree representation;
they are not a summary over all optimal reconstructions. Isolated versus clustered discordance
is descriptive evidence, not a classification of sequencing errors or admixture.

```bash
alignment-info alignment.fasta tree.newick --site-parsimony --state-groups > groups.tsv
```

# EXAMPLES

For a reproducible example, create `example.fasta`:

```
>a
AAAA
>b
ATAA
>c
TT-A
>d
TT-A
```

And `example.tree`:

```
(a,b,(c,d));
```

Request ordinary reports with and without a tree:

```bash
alignment-info example.fasta --alphabet DNA
alignment-info example.fasta example.tree --alphabet DNA
```

The example has four columns and four sequences. Without indels, two columns are nonconstant
and one is informative; including indels gives three and two, respectively. Mean mismatch fractions
are approximately 0.368 without indels and 0.478 with indels. There is one retained gap-run group with count
two. The tree's unit-cost parsimony total is two. Despite no observed C or G calls, each has
reported frequency 9.09 percent because of the pseudocounts.

List input names, raw-character lengths, or both:

```bash
alignment-info example.fasta --show-names
alignment-info example.fasta --show-lengths
alignment-info example.fasta --show-names --show-lengths
```

The combined output is `a,4`, `b,4`, `c,3`, and `d,3`, one per line.

Remove empty columns explicitly, or read an alignment from standard input:

```bash
alignment-info example.fasta --alphabet DNA --erase-empty-columns
alignment-info - example.tree --alphabet DNA < example.fasta
```

The example has no empty columns, so removal does not alter it. In per-site mode:

```bash
alignment-info example.fasta example.tree --alphabet DNA --site-parsimony
```

The TSV output is:

```text
column  n_called  n_states  parsimony
1       4         2         1
2       4         2         1
3       2         1         0
4       4         1         0
```

Spacing above is for readability; the actual delimiters are tabs.


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

