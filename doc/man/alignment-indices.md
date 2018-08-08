% alignment-indices(1)
% Benjamin Redelings
% Feb 2018

# NAME

**alignment-indices** - Show the alignment in terms of the index of each character in its sequence.

# SYNOPSIS

**alignment-indices** _alignment-file_ [OPTIONS]

# DESCRIPTION

Show the alignment in terms of the index of each character in its sequence.

# ALLOWED OPTIONS:
**-h**, **--help**
: produce help message

**--align** _arg_
: file with sequences and initial alignment

**--alphabet** _arg_
: set to 'Codons' to prefer codon alphabets

**-c** _arg_, **--columns** _arg_
: Ranges of columns to keep, like: 1-10,30-

**--invariant** _arg_
: print only sites where this site and <arg> neighbors are invariant.

**--differences** _arg_
: how many sequences may differ from the majority?

**--avoid-gaps** _arg_ (=3)
: How far from a gap must a column be to be invariant?

**--min-constraints** _arg_
: Minimum # of constraints per column for a constraint to be emitted


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

