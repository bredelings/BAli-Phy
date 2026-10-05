% alignment-find(1)
% Benjamin Redelings
% October 2026

# NAME

**alignment-find** - Extract the first or last FASTA alignment from a stream.

# SYNOPSIS

**alignment-find** \[OPTIONS\] < _alignments-file_ > _alignment.fasta_

# DESCRIPTION

Read standard input and write one alignment in FASTA format to standard output.
By default, select the last alignment; use **`--first`** to select the first.
Input filenames are not command-line arguments: use shell input redirection or a pipe.

An alignment starts with a FASTA header whose first character is **>** and ends at
an empty line or the end of input. Separate successive alignments with empty lines.
Text before an alignment, or between alignments, is skipped while searching for the
next header. This allows extraction from a stream containing sample labels and FASTA
alignment blocks. PHYLIP input is not supported by this tool.

Sequence letters are converted to uppercase, and spaces and tabs within sequences
are removed. Sequence lengths and all columns are preserved, including columns
containing only **-**, **?**, or **=**. Symbols are not interpreted using an alphabet.
The output is FASTA with normalized formatting.

With **`--first`**, reading stops after the first alignment. With **`--last`**, the
stream is read until its end or an alignment cannot be loaded. A loading error
produces a warning on standard error and stops the search; if an earlier alignment
was successfully loaded, that alignment is returned. Thus successful exit does not
necessarily mean that every input alignment was valid. If no alignment was loaded,
the program reports an error.

# OPTIONS

**-h**, **`--help`**
: Print usage information and exit.

**`--first`**
: Select the first alignment. Cannot be combined with **`--last`**.

**`--last`**
: Select the last successfully loaded alignment. This is the default when neither
  selection flag is given. Cannot be combined with **`--first`**.

# EXAMPLES

Extract the last alignment in a sample file:

```
alignment-find < samples.fastas > last.fasta
```

Extract the first alignment:

```
alignment-find --first < samples.fastas > first.fasta
```

Convert the selected alignment to PHYLIP:

```
alignment-find < samples.fastas | alignment-cat -o phylip > last.phy
```

# EXIT STATUS

Returns 0 on success (including **`--help`**) and 1 when an error is reported.
Warnings and error messages are written to standard error.

# SEE ALSO

**alignment-cat**(1), **alignment-chop-internal**(1), **cut-range**(1)

# REPORTING BUGS

BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
