% alignment-thin(1)
% Benjamin Redelings
% Feb 2018

# NAME

**alignment-thin** - Remove sequences or columns from an alignment.

# SYNOPSIS

**alignment-thin** _alignment-file_ [OPTIONS]

# DESCRIPTION

Remove sequences or columns from an alignment.

# GENERAL OPTIONS:
**-h**, **--help**
: Print usage information.

**-v**, **--verbose**
: Output more log messages on stderr.


# SEQUENCE FILTERING OPTIONS:
**--cutoff** _arg_
: Keep only sequence with more mismatches than _arg_.

**--longer-than** _arg_
: Keep only sequences longer than _arg_.

**--shorter-than** _arg_
: Keep only sequence sequences shorter than _arg_.

**--keep** _arg_
: Keep only sequences in comma-separated list _arg_.

**--remove** _arg_
: Remove sequences in comma-separated list _arg_.

**--down-to** _arg_
: Remove similar sequences down to _arg_ sequences.

**--remove-crazy** _arg_
: Remove _arg_ sequences that are missing too many conserved sites.

**--conserved** _arg_ (=0.75)
: Fraction of sequences that must contain a letter for it to be considered conserved.


# COLUMN FILTERING OPTIONS:
**--min-letters** _arg_
: Remove columns with fewer than _arg_ letters.

**--remove-unique** _arg_
: Remove insertions in a single sequence if longer than _arg_ letters


# OUTPUT OPTIONS:
**--sort**
: Sort partially ordered columns to group similar gaps.

**--show-lengths**
: Just print out sequence lengths.

**--find-dups** _arg_
: For each sequence, find the closest other sequence.


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

