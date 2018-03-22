% alignments-diff(1)
% Benjamin Redelings
% Feb 2018

# NAME

**alignments-diff** -- Align two alignments for comparison.

# SYNOPSIS

**alignments-diff** alignment-file1 alignment-file2 [OPTIONS]

# DESCRIPTION

Align two alignments for comparison.

# GENERAL OPTIONS:
**-h**, **--help**
: produce help message


# INPUT OPTIONS:
**--alphabet** _arg_
: set to 'Codons' to prefer codon alphabets


# OUTPUT OPTIONS:
**--merge**
: Stack the two alignments into one alignment with duplicate names

**--dual**
: Write out the two aligned alignments separately

**--fill** _arg_ (=gap)
: blank columns filled with: gap or unknown

**-d** _arg_, **--differences-file** _arg_
: Filename to store differences in AU format


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

