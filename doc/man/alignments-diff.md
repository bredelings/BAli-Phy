% alignments-diff(1)
% Benjamin Redelings
% October 2026

# NAME

**alignments-diff** - Compare the placement of residues in two alignments.

# SYNOPSIS

**alignments-diff** \[OPTIONS\] _ALIGNMENT1_ _ALIGNMENT2_

# DESCRIPTION

Compare two alignments of the same sequences to show where their alignment of residues differs.
A residue is identified by its position in its sequence, so repeated copies of the same letter
are distinct. The program matches columns while preserving their order, favoring matches that
place the same residues together.

By default, write annotations for coloring the first alignment with **alignment-draw**. This
provides a visual comparison of alternative alignment estimates. With **--dual** or **--merge**,
write the alignments themselves with extra blank columns inserted to line up corresponding
residues.

# INPUT

ALIGNMENT1 and ALIGNMENT2 are required positional filenames. Each contains one alignment.
Both must contain the same sequence names and the same sequences after removing gaps.
Sequence order may differ; the second alignment is reordered to match the first.

# OUTPUT

Output goes to standard output; diagnostics go to standard error.

The default output is a space-separated annotation table for ALIGNMENT1. Its header lists
sequence names, followed by one data row per column of that alignment. Each row has one value
per sequence and an additional, unlabelled value for the whole column:

- A residue has value 0 if it matches the residue in the corresponding column of ALIGNMENT2.
  Otherwise it receives a category from 1 to **--classes**, based on its position in ALIGNMENT2.
  Gaps and unknown cells also have value 0.
- The final value is 1 if the entire column matches, including gaps, and 0 otherwise.

These are comparison categories, not probabilities. Use **--scale identity** and a **diff[N]**
color scheme in **alignment-draw**, with N equal to **--classes**.

With **--dual**, write two FASTA alignments with corresponding columns, separated by empty lines.
With **--merge**, write one FASTA alignment: the first alignment's rows, a blank separator row,
and the second alignment's rows. Each name from the second alignment has `2` appended.

With **--merge --differences-file**, also write an annotation table for the merged alignment to
the specified file. In this table, a residue receives 1 when the same residue occurs in the
corresponding column of both alignments, and 0 otherwise. Separator and whole-column values
are 0. This table records agreement rather than the difference categories of the default output.

# OPTIONS

**-h**, **--help**
: Print help and exit.

**--alphabet** _ALPHABET_
: Specify the sequence alphabet instead of inferring it from input, for example `DNA`,
  `Amino-Acids`, or `Codons`.

**--merge**
: Write a combined FASTA alignment containing both sets of rows. Takes precedence over **--dual**.

**--dual**
: Write the two alignments separately in FASTA format, with corresponding columns.

**--fill** _KIND_
: Fill inserted columns and the merged separator row with `gap` or `unknown`. Default: `gap`.

**-d**, **--differences-file** _FILE_
: Write agreement annotations for the merged alignment to FILE. Used with **--merge**.

**--blocksize** _N_
: Divide ALIGNMENT2 into blocks of N columns when assigning difference categories. Default: 20.

**--classes** _N_
: Cycle through N difference categories for successive blocks of ALIGNMENT2. Default: 1.
  Together with **--blocksize**, this controls the default annotation output.

# EXAMPLES

Highlight differences on the first alignment:

    alignments-diff first.fasta second.fasta > differences.txt
    alignment-draw first.fasta differences.txt --scale identity \
      --color-scheme 'diff[1]' --show-ruler > comparison.html

Write both alignments with corresponding columns:

    alignments-diff --dual first.fasta second.fasta > paired.fastas

Combine the alignments and save agreement annotations:

    alignments-diff --merge --differences-file agreement.txt \
      first.fasta second.fasta > merged.fasta

# SEE ALSO

**alignment-draw**(1), **alignment-compare**(1)

# REPORTING BUGS

BAli-Phy online help: <https://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
