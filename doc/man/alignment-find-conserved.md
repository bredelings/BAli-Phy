% alignment-find-conserved(1)
% Benjamin Redelings
% September 2026

# NAME

**alignment-find-conserved** - Highlight conserved groups and report site-wise parsimony.

# SYNOPSIS

**alignment-find-conserved** [_alignment-file_] **`--tree`** _tree-file_ \[OPTIONS\]

**alignment-find-conserved** **`--align`** _alignment-file_ **`--tree`** _tree-file_ \[OPTIONS\]

# DESCRIPTION

Produce highlighting values for each alignment column and sequence, based on conservation
within named groups and differences between groups. Also report group conservation values,
inferred ancestral states, and the minimum number of substitutions for each column.

The highlighting matrix is written to standard output. Group listings and per-column
parsimony diagnostics are written to standard error; redirect the two streams separately.
This command does not estimate a tree, branch lengths, substitution rates, or support values.

Supply an alignment and a matching Newick tree. Sequence names are matched to tree-tip names;
sequences may be reordered to match the tree. The output header gives the resulting order.
A tree is required for the calculations, although the option parser does not enforce this.
Groups are optional: without them the highlighting matrix is zero, but per-column parsimony
scores are still reported.

Empty columns are removed during alignment loading, including columns consisting entirely
of gaps (`-`), unknown calls (`?`), or a mixture of these. All-`N` DNA columns are retained. Reported positions are 1-based indices in the processed alignment, not necessarily
original alignment columns or reference-sequence coordinates. For Triplets or Codons they
index alphabet units rather than individual nucleotides.

# GROUPS AND SPLITS

A group file contains one nonempty group per line, with a name ending in a colon followed
by space-separated sequence names. For example:

```
left: a b c
right: d e f
```

Use simple names without spaces, and do not include blank lines or comments. Every sequence
name must exist in the alignment. Each group must be exactly the set of tips on one side of
a tree edge; a group that does not match an edge causes an error. Groups need not cover all
sequences and may overlap if they satisfy that condition. Group order in diagnostics is
file order.

The **`--split`** option specifies comparisons between these group names, not sequence names:

```
--split 'left | right'
```

Separate names and the `|` with spaces, and quote the whole argument for the shell. Either
side can contain multiple group names. A group cannot occur on both sides of one split.
Groups omitted from a split do not participate in that comparison. Repeat **`--split`** for
multiple comparisons; a group can qualify through any one of them.

# CONSERVATION AND HIGHLIGHTING

For a group of _n_ sequences at a column, find the most frequent encoded symbol and its
count _k_. The group has a conservation value only if all three conditions hold:

- _k_ is at least 3;
- _k_ is at least _n_ - 1;
- _k_ is greater than _n_ / 2.

Thus a three-member group must be unanimous, a four-member group needs three matches, and
a larger group allows at most one exception. Groups with fewer than three members cannot
qualify. These tests compare encoded symbols literally; they do not distribute ambiguous
calls among the nucleotides those calls could represent. Gaps and missing symbols are not
omitted from the group size or symbol counts. The gap symbol `-` is also used to indicate
that no conservation value was assigned, so conservation at a gap is not distinguished
from lack of conservation.

Initially every group is considered interesting. The requirement options below filter this
status; multiple requirements must all be satisfied. With **`--split`**, a group must also
belong to at least one specified split.

Each sequence receives a highlighting value for each column:

- **1** if it belongs to an interesting group;
- **0.5** if it belongs to a group with a non-gap conservation value but no interesting group;
- **0** otherwise.

Overlapping groups contribute the maximum applicable value. Conserved groups retain the
0.5 highlight even when they fail a requirement for being interesting. With no requirement
options, all members of eligible groups receive 1, whether conserved or not.

# OPTIONS

**-h**, **`--help`**
: Print help and exit. The current help text uses the old spelling `alignment-find conserved`;
  invoke the executable as **alignment-find-conserved**.

**`--align`** _file_
: Read the alignment from _file_, instead of supplying it positionally. The default is `-`,
  meaning standard input.

**`--tree`** _file_
: Read the tree used to match groups to edges and calculate parsimony. Required in practice.

**`--alphabet`** _name_
: Specify the alphabet instead of inferring it. Examples include `DNA`, `RNA`, `Amino-Acids`,
  `Triplets`, and `Codons`.

**`--groups`** _file_
: Read named taxon groups in the format described above.

**`--split`** _comparison_
: Restrict interesting groups to specified comparisons, for example `'left | right'`.
  May be repeated. Alone, this restricts group membership; it does not require differences.

**`--require-conservation`**
: Require a non-gap conservation value for a group to be interesting.

**`--require-change`**
: Without **`--split`**, require that the group conservation values are not all equal. If this
  holds, all groups pass this particular filter. A conserved versus unconserved difference
  counts unless **`--ignore-rate-change`** is also supplied.

  With **`--split`**, a split qualifies when at least one cross-side group pair has different
  conservation values and no cross-side pair has equal values. Groups on either side of
  a qualifying split pass this filter. The unconserved marker participates in these comparisons.

**`--ignore-rate-change`**
: Modify **`--require-change`**. Without splits, ignore unconserved groups when testing whether
  conservation values differ. With splits, require at least one cross-side pair with different
  non-gap conservation values; the rule excluding any equal cross-side pair still applies.
  This option has no effect without **`--require-change`** and does not estimate evolutionary rates.

**`--require-all-different`**
: Require disjoint sets of observed encoded symbols between groups. This compares all members,
  not just conservation values, and does not require that either group be conserved.

  Without **`--split`**, any disjoint pair makes all groups pass this filter. With **`--split`**,
  every cross-side pair must be disjoint for that split to qualify; its participating groups
  then pass the filter. Gaps, missing symbols, and ambiguity codes are compared literally,
  not by overlap of their possible nucleotide states.

# OUTPUT

## Standard output

The first line lists sequence names separated by spaces. Each following line corresponds
to one processed alignment column and contains space-separated highlighting values in that
sequence order. There is no position column.

The current implementation writes **one extra trailing zero** on every numerical row, with
no corresponding name in the header. This is an existing output quirk, not another sequence.

## Standard error

When groups are supplied, their names and member sequences are listed first. For each
processed column the command then prints:

1. The 1-based column number, the conservation value for each group (`-` for no value), and
   one interesting-status flag per group (`1` or `0`).
2. For each group, a concatenated set of states inferred at its attachment node, followed
   by the minimum substitution count for the entire column.
3. A blank line.

State sets include alternatives supported by minimum-cost ancestral reconstructions. They
are not posterior probabilities. Without groups, the first line contains only the column
number and the second contains only the score. Errors also go to standard error, so this
stream is diagnostic output rather than a dedicated tabular score interface.

Parsimony assigns cost zero to equal states and one to unequal states in the selected
alphabet; branch lengths are ignored. Gaps and fully unknown calls do not constrain the
ancestral reconstruction. Partial ambiguity codes constrain a call to the states they allow.
This differs from the literal-symbol comparisons used for group highlighting.

For a site with exactly two observed states and otherwise fully missing calls, a score of
1 means an edge separates the two state groups. A larger score requires multiple changes
on this tree. A score of 0 means no change is needed, as for a constant site. The score alone
does not identify sequencing errors, admixture, or an incorrect tree.

# EXAMPLES

Create `example.fasta`:

```
>a
AAA
>b
ATA
>c
AAA
>d
TTA
>e
TTA
>f
TTA
```

Create `example.tree`:

```
((a,b),c,((d,e),f));
```

Create `groups.txt` using the `left` and `right` groups shown above. Highlight conserved groups:

```bash
alignment-find-conserved example.fasta --tree example.tree \
    --groups groups.txt --require-conservation \
    > highlights.txt 2> diagnostics.txt
```

Both groups receive 1 at columns 1 and 3. At column 2, only `right` is conserved and receives
1; `left` receives 0. Every numerical row also has the extra trailing zero described above.

Require different conserved values across the split:

```bash
alignment-find-conserved example.fasta --tree example.tree \
    --groups groups.txt --split 'left | right' \
    --require-change --ignore-rate-change \
    > changes.txt 2> changes.log
```

Column 1 receives 1 in both groups. At column 2, `left` receives 0 and `right` receives 0.5.
At column 3 both groups receive 0.5: they are conserved at the same state, so neither is
interesting under this comparison.

Obtain per-column parsimony diagnostics without groups:

```bash
alignment-find-conserved example.fasta --tree example.tree \
    > /dev/null 2> site-scores.txt
```

The scores for columns 1, 2, and 3 are respectively 1, 2, and 0. They are printed on the lines
following the corresponding column numbers, not alongside them in a two-column table.

# SEE ALSO

**alignment-info**(1), **alignment-draw**(1), **tree-tool**(1)

# REPORTING BUGS

BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
