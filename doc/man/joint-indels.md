% joint-indels(1)
% Benjamin Redelings
% Feb 2018

# NAME

**joint-indels** - Usage: joint-indels <alignments file> <trees file> [OPTIONS]

# SYNOPSIS

**joint-indels** _alignments file_ _trees file_ [OPTIONS]

# DESCRIPTION

Usage: joint-indels <alignments file> <trees file> [OPTIONS]

# ALLOWED OPTIONS:
**-h**, **--help**
: produces help message

**--subsample** _arg_ (=10)
: factor by which to sub-sample trees

**--partition** _arg_
: find indels along internal branch that bi-partitions given taxa (_taxa1_:_taxa2_:...)

**--alphabet** _arg_
: set to 'Codons' to prefer codon alphabets

**-V**, **--verbose**
: \[=arg(=1)\] Show more log messages on stderr.

**--extract-sequences** _arg_
: Extract sequences corresponding to tree

**--details**
: Output detailed indel information (Start/End positions, I/D type, lengths).
  See <https://github.com/bredelings/BAli-Phy/blob/master/tests/tools/joint-indels/README.md> for output format documentation.


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

