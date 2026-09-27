% alignment-cat(1)
% Benjamin Redelings
% September 2026

# NAME

**alignment-cat** - Concatenate, select, reorder, and reformat aligned sequences.

# SYNOPSIS

**alignment-cat** \[OPTIONS\] [_file_ ...]

# DESCRIPTION

Read sequences in FASTA or PHYLIP format and write the result to standard output.
The input format is detected automatically. With multiple input files, concatenate
alignments end-to-end in command-line order, matching sequences by name rather than
by their position in each file. With a single input, select or transform its sequences.
The default output format is FASTA.

In FASTA input, the sequence name is the text immediately following **>**, up to
the first space or tab; the rest of the header is a comment. Sequence letters are
converted to uppercase when read. Sequence names retain their case.

If no input file is given, read standard input. A filename of **-** also denotes
standard input. If **-** occurs more than once, the same input is reused each time.

By default, concatenated alignments must contain the same sequence names, and the
output follows the sequence order of the first file. Sequence selection options
can instead specify the names and order to use in every input.

When concatenating multiple files, all sequences within each input must have the
same length; different files may have different alignment lengths. Use **`--pad`**
to append gaps to shorter sequences within each file. This length check occurs
before sequence selection, including for sequences that will be discarded.
A single FASTA input may contain sequences of unequal length.

# OPTIONS

Long option names must be written in full; abbreviations are not accepted.
Each option may be supplied once, except **`--file`**, which may be repeated to
specify input files as an alternative to positional filenames.

**-h**, **`--help`**
: Print usage information and exit.

**`--output`** _format_
: Write **fasta** (the default) or **phylip**. PHYLIP output is interleaved and
  truncates sequence names to 10 characters; names should remain unique after
  truncation. Use PHYLIP only when the final sequences have equal lengths.

**-c** _ranges_, **`--columns`** _ranges_
: Keep the specified columns, for example **1-10,30-**. Positions are numbered
  from 1 in the concatenated alignment, after **`--align-by-amino`** if requested.
  See COLUMN SELECTION below for range syntax and unequal-length inputs.

**-t** _names_, **`--taxa`** _names_
: Keep only the named sequences, in the order listed. Separate names with commas,
  or use `@filename` to read names separated by commas or newlines from a file.
  Names are case-sensitive and must be present in every input alignment. Spaces
  around names are not removed: use `human,mouse`, not `human, mouse`.
  This option takes precedence over both reordering options.

**-p**, **`--pad`**
: Append **-** characters to shorter sequences so that all sequences within each
  input file have the length of that file's longest sequence. Later operations
  such as **`--strip-gaps`** may make the lengths unequal again.

**-r**, **`--reverse`**
: Reverse the characters in each sequence after all other transformations.
  Nucleotides are not complemented.

**-e**, **`--erase-empty-columns`**
: Remove columns consisting entirely of characters in **`--missing`** (by default,
  **-** and **?**). This examines the retained sequences after column selection.
  Sequences must have equal lengths at this stage.

**`--missing`** _characters_
: Set the characters treated as gaps or missing data by **`--erase-empty-columns`**,
  **`--strip-gaps`**, and **`--align-by-amino`**. The default is **-?**. The value is a
  literal list of characters, not a regular expression, and replaces the default
  list. Matching is case-sensitive, so use uppercase letters to match the sequence
  data: **`--missing='-?N'`** also treats **N** as missing. This option does not
  change the **-** character inserted by **`--pad`**.

**`--strip-gaps`**
: Remove every character in **`--missing`** from each sequence independently.
  The result may have unequal sequence lengths and no longer preserve alignment
  columns; use FASTA output for such sequences.

**`--reorder-by-tree`** _tree-file_
: Select and order sequences using the leaf names of a Newick tree. Every leaf
  name must occur in every input alignment; sequences absent from the tree are
  discarded. By default, the program chooses a root using branch lengths.
  At each node, subtrees with fewer edges on their longest path to a leaf come
  first, with ties broken by the alphabetically first leaf name in each subtree.
  Thus the output need not follow
  the order in which names appear in the Newick file. This option takes precedence
  over **`--reorder-by-alignment`**, but is ignored when **`--taxa`** is supplied.

**`--use-root`**
: With **`--reorder-by-tree`**, retain the root specified in the tree file instead
  of choosing a root automatically.

**`--reorder-by-alignment`** _alignment-file_
: Select and order sequences using the names in another FASTA or PHYLIP alignment.
  Every reference name must occur in every input alignment; other sequences are
  discarded. Only the reference names and their order are used, not its columns.
  Ignored when **`--taxa`** or **`--reorder-by-tree`** is supplied.

**`--align-by-amino`** _amino-acid-alignment_
: Arrange nucleotide sequences into a codon alignment using a FASTA or PHYLIP
  amino-acid alignment. Sequences are matched by name, and the output follows
  the amino-acid alignment's order. See CODON ALIGNMENT below.

**`-V[LEVEL]`**, **`--verbose[=LEVEL]`**
: Set diagnostic verbosity. Omitting _level_ sets it to 1. Diagnostics are written
  to standard error, including rooting information when a root is chosen for
  **`--reorder-by-tree`**.

# PROCESSING ORDER

Transformations occur in this order, regardless of their order on the command line:

1. Pad each input if requested, check lengths for concatenation, select and order
   sequences, and concatenate the inputs.
2. Arrange nucleotides using **`--align-by-amino`**.
3. Select **`--columns`**.
4. Remove empty columns with **`--erase-empty-columns`**.
5. Remove missing characters with **`--strip-gaps`**.
6. Reverse each sequence with **`--reverse`**, then write the output.

Thus empty-column removal considers only the retained taxa and columns, and
column numbers always refer to positions before gap stripping or reversal.

# COLUMN SELECTION

The **`--columns`** argument is a comma-separated list of positions or inclusive
ranges. For example, **1-10,30-** keeps columns 1 through 10 followed by column 30
through the end. An omitted start means column 1. A positive step after **/**
selects columns at that interval from the start of the range: **2-/3** selects
columns 2, 5, 8, and so on. Ranges are appended in the order given, including
repeated columns. Range endpoints must lie within the alignment.

For unequal-length sequences, ranges are checked against the longest sequence.
For each shorter sequence, selection stops at the first requested column beyond
its end, even if later requested columns would be within it. Use **`--pad`** before
selecting columns to give all sequences the same coordinate range.

# CODON ALIGNMENT

For **`--align-by-amino`**, supply nucleotide sequences in the reading frame of the
amino-acid alignment. The nucleotide sequences, after concatenation and before
removing missing characters, must each have a length divisible by three.
Ungapped coding sequences are the simplest input.

The amino-acid alignment must contain the same number of sequences and matching
names as the selected nucleotide input. Shorter amino-acid sequences are padded
with **-** to the length of the longest sequence, even without **`--pad`**.

Characters listed in **`--missing`** are removed from each nucleotide sequence.
Each non-missing amino acid consumes the next three nucleotides, and each missing
amino-acid character is copied three times: for example, **-** becomes `---` and
**?** becomes **???**. Keep **-** in **`--missing`** so that padding in the amino-acid
alignment is treated as gaps.

Supply exactly three non-missing nucleotides per non-missing amino acid. The
program checks the number of complete triplets after removing missing characters,
but can silently discard one or two trailing nucleotides when that count matches.
It does not translate the nucleotides or check that their codons encode the
supplied amino acids.

# EXAMPLES

Concatenate two alignments, matching sequences by name:

```
alignment-cat gene1.fasta gene2.fasta > combined.fasta
```

Convert standard input to PHYLIP:

```
alignment-cat --output=phylip < alignment.fasta > alignment.phy
```

Keep selected columns from an alignment of at least 600 columns:

```
alignment-cat --columns=1-10,50-100,600- alignment.fasta > selected.fasta
```

Extract the first and second positions of aligned codons starting at column 1:

```
alignment-cat --columns=1-/3 codons.fasta > position1.fasta
alignment-cat --columns=2-/3 codons.fasta > position2.fasta
```

Keep two sequences in the specified order, then remove columns empty in both:

```
alignment-cat --taxa=human,mouse --erase-empty-columns alignment.fasta > subset.fasta
```

Read the sequence selection and order from a file containing one name per line:

```
alignment-cat --taxa=@names.txt alignment.fasta > subset.fasta
```

Pad each input before concatenation:

```
alignment-cat --pad gene1.fasta gene2.fasta > combined.fasta
```

Remove gaps and missing-data characters from each sequence:

```
alignment-cat --strip-gaps alignment.fasta > sequences.fasta
```

Order sequences using a tree's specified root:

```
alignment-cat --reorder-by-tree=tree.nwk --use-root alignment.fasta > ordered.fasta
```

Construct a codon alignment from coding sequences and aligned proteins:

```
alignment-cat --align-by-amino=proteins.fasta coding.fasta > codons.fasta
```

# EXIT STATUS

Returns 0 on success (including **`--help`**) and 1 when an error is reported.
Error messages are written to standard error.

# SEE ALSO

**alignment-thin**(1), **alignment-translate**(1), **alignment-info**(1)

# REPORTING BUGS

BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
