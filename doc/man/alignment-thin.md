% alignment-thin(1)
% Benjamin Redelings
% Feb 2018

# NAME

**alignment-thin** -- Remove sequences or columns from an alignment.

# SYNOPSIS

**alignment-thin** _alignment-file_

# DESCRIPTION

Remove sequences or columns from an alignment.

# ALLOWED OPTIONS:
**-h**, **--help**
: produce help message

**--align** _arg_
: file with sequences and initial alignment

**--find-dups** _arg_
: for each other sequence, find the closest sequence

**--cutoff** _arg_
: only leave taxa with more mismatches than this value

**--longer-than** _arg_
: only leave taxa w/ sequences longer than this

**--shorter-than** _arg_
: only leave taxa w/ sequences shorter than this

**--down-to** _arg_
: number of taxa to keep

**--keep** _arg_
: comma-separated list of taxon names to keep - remove others

**--min-letters** _arg_
: Remove columns with fewer letters.

**-v**, **--verbose**
: Output more log messages on stderr.

**--show-lengths**
: just print out sequence lengths

**--sort**
: Sort partially ordered columns to minimize the number of visible indels.

**--remove-unique** _arg_
: Remove insertions in a single sequence if longer than this many letters

**--remove-crazy** _arg_
: Remove sequence that have deleted conserved sites

**--remove** _arg_
: comma-separated list of taxon names to remove

**--conserved-fraction** _arg_ (=0.75)
: Fraction of sequences that must contain a letter for it to be considered conserved.


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

