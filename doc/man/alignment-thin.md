% alignment-thin(1)
% Benjamin Redelings
% October 2026

# NAME

**alignment-thin** - Remove sequences or columns from an alignment.

# SYNOPSIS

**alignment-thin** \[OPTIONS\] _alignment-file_

# DESCRIPTION

Read an alignment, remove selected sequences or columns, and write the resulting alignment in
FASTA format to standard output. The alignment filename is required; use `-` to read standard
input. Diagnostics and **`--find-dups`** reports go to standard error.

Sequence filtering precedes column filtering. Length filters run first, followed by selection
by name, removal of gappy sequences, and similarity thinning. Column filters therefore use the
remaining sequences. Empty columns are retained unless a column filter removes them.

**`--protect`**, **`--keep`**, and **`--remove`** accept comma-separated names or `@filename`.
In a names file, separate names with commas or newlines. Names are matched exactly. To supply
an initial literal `@`, escape it as `\@` and quote the argument for the shell.

Protected sequences survive all sequence filters. **`--keep`** also protects the listed
sequences and retains any sequences named by **`--protect`**, even if a length filter would
remove them. **`--keep`** and **`--remove`** cannot be given together.

To list sequence names or lengths, use **`alignment-info --show-names`** or
**`alignment-info --show-lengths`** instead.

# GENERAL OPTIONS

**-h**, **`--help`**
: Print usage and options, then exit.

**-V**, **`--verbose`**
: Output more log messages on standard error.

# SEQUENCE FILTERING OPTIONS

**-p** _names_, **`--protect`** _names_
: Protect the listed sequences from removal, without excluding other sequences.

**-k** _names_, **`--keep`** _names_
: Keep only the listed sequences and any sequences named by **`--protect`**. These sequences
  are protected from subsequent sequence filters.

**-r** _names_, **`--remove`** _names_
: Remove the listed sequences unless they are protected.

**-l** _length_, **`--longer-than`** _length_
: Remove unprotected sequences with ungapped length less than or equal to _length_.

**-s** _length_, **`--shorter-than`** _length_
: Remove unprotected sequences with ungapped length greater than or equal to _length_.

**-c** _count_, **`--cutoff`** _count_
: Thin similar sequences using a strict cutoff of fewer than _count_ directional mismatches.
  Count positions where the candidate has a present character other than a fully unknown
  state (such as DNA `N`) and the other sequence has a different symbol, including a gap.
  The count can differ when the two sequences are exchanged. Protected sequences are never
  removed.

**-d** _count_, **`--down-to`** _count_
: Thin similar sequences toward _count_ retained sequences. Earlier sequence removals count
  toward this target. Protection or the absence of a removable pair can prevent reaching it.
  When combined with **`--cutoff`**, the result can contain fewer than _count_ sequences to
  satisfy the similarity cutoff.

**`--remove-gappy`** _count_
: Remove up to _count_ unprotected sequences with the fewest present characters at conserved
  columns. Conserved columns are recalculated among the remaining sequences after each removal.

**`--conserved`** _fraction_ (=0.75)
: For **`--remove-gappy`**, the minimum fraction of retained sequences with a present character
  needed to classify a column as conserved. Conservation here refers to occupancy, not identity.

# COLUMN FILTERING OPTIONS

**-K** _name_, **`--keep-columns`** _name_
: Protect columns occupied by the named sequence from **`--min-letters`** and
  **`--remove-unique`**. The sequence is looked up in the original alignment, even if removed
  by a sequence filter. Does not override **`--erase-empty-columns`**.

**-m** _count_, **`--min-letters`** _count_
: Remove unprotected columns with fewer than _count_ present characters among retained sequences.

**-u** _length_, **`--remove-unique`** _length_
: Shorten runs of characters present in only one retained sequence to at most _length_
  characters, subject to **`--keep-columns`** protection. Internal runs are shortened from the
  middle; terminal runs are shortened from the outer end.

**-e**, **`--erase-empty-columns`**
: Remove columns with no present characters after the other column filters. Gaps (`-`) and
  unknown calls (`?`) do not count as present; ambiguous non-gap states such as DNA `N` do.

# OUTPUT OPTIONS

**-S**, **`--sort`**
: Reorder columns to group similar gaps while preserving the character order within each
  sequence. Applied after filtering.

**-F** _names_, **`--find-dups`** _names_
: For each input sequence outside the comma-separated target list, report its nearest target
  on standard error. The list must be nonempty; `@filename` is not supported for this option.
  Targets are chosen by mismatches at positions where both sequences have present characters
  other than fully unknown states such as DNA `N`.
  Reports use the original alignment, before filtering, and do not replace the alignment
  written to standard output.

# EXAMPLES

Keep only selected sequences:

```sh
alignment-thin --keep=seq1,seq2 file.fasta > selected.fasta
```

Remove named sequences, or sequences of length 250 or less:

```sh
alignment-thin --remove=seq1,seq2 file.fasta > filtered.fasta
alignment-thin --longer-than=250 file.fasta > long.fasta
```

Thin to 30 sequences while protecting names listed in a file:

```sh
alignment-thin --down-to=30 --protect=@names.txt file.fasta > thinned.fasta
```

Thin sequences using a cutoff of five directional mismatches:

```sh
alignment-thin --cutoff=5 file.fasta > thinned.fasta
```

Remove up to ten sequences with poor coverage of conserved columns:

```sh
alignment-thin --remove-gappy=10 file.fasta > filtered.fasta
```

Keep columns with at least five present characters, retaining columns occupied by `reference`:

```sh
alignment-thin --min-letters=5 --keep-columns=reference file.fasta > columns.fasta
```

Remove empty columns from standard input:

```sh
alignment-thin --erase-empty-columns - < file.fasta > compact.fasta
```

Report each non-target sequence's nearest target, discarding the alignment output:

```sh
alignment-thin --find-dups=seq1,seq2 file.fasta > /dev/null
```

# SEE ALSO

**alignment-cat**(1), **alignment-info**(1)

# REPORTING BUGS

BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
