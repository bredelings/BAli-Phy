% alignment-cat(1)
% Benjamin Redelings
% Feb 2018

# NAME

**alignment-cat** - Concatenate several alignments (with the same sequence names) end-to-end.

# SYNOPSIS

**alignment-cat** _file1_ [_file2_ ...]

# DESCRIPTION

Concatenate several alignments (with the same sequence names) end-to-end.

# ALL OPTIONS:
**-h**, **--help**
: Produce help message

**--output** _arg_ (=fasta)
: Which output format: fasta or phylip?

**-c** _arg_, **--columns** _arg_
: Ranges of columns to keep, like: 1-10,30-

**-t** _arg_, **--taxa** _arg_
: Taxa to keep, comma-separated

**-p**, **--pad**
: Add gaps to make sequence lengths identical

**-r**, **--reverse**
: Reverse the sequences

**-e**, **--erase-empty-columns**
: Remove columns with no characters (all gaps).

**--missing** _arg_ (=-?)
: What letters are not characters (e.g. gaps)?

**--strip-gaps**
: Remove all non-character letters from sequences.

**--reorder-by-tree** _arg_
: Reorder the sequences given a tree

**--use-root**
: use the root specified in the tree file to reorder

**--reorder-by-alignment** _arg_
: Reorder the sequences following an alignment

**--align-by-amino** _arg_
: Arrange nucleotides into codon alignment


# EXAMPLES:
 



To select columns from an alignment:

% alignment-cat -c1-10,50-100,600- filename.fasta > result.fasta

% alignment-cat -c5-250/3 filename.fasta > first_codon_position.fasta

% alignment-cat -c6-250/3 filename.fasta > second_codon_position.fasta



To concatenate two or more alignments:

% alignment-cat filename1.fasta filename2.fasta > all.fasta

# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

