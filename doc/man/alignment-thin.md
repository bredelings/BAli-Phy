% alignment-thin(1)
% Benjamin Redelings
% Feb 2018

# NAME

**alignment-thin** -- Remove sequences or columns from an alignment.

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

