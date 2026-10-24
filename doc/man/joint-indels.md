% joint-indels(1)
% Benjamin Redelings
% Feb 2018

# NAME

**joint-indels** - Usage: joint-indels <alignments file> <trees file> [OPTIONS]

# SYNOPSIS

**joint-indels** _ALIGNMENTS_ _TREES_ [OPTIONS]

# DESCRIPTION

_ALIGNMENTS_ and _TREES_ are required positional filenames containing alignment
samples and corresponding tree samples, respectively.

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
: Show more log messages on stderr. Repeat the flag or use `--verbose=2` for more detail.

**--extract-sequences** _arg_
: Extract sequences corresponding to tree

**--details**
: Output detailed indel information (Start/End positions, I/D type, lengths).
  Positions and lengths count ungapped alphabet symbols (one symbol per codon with
  a codon alphabet). Start counts symbols before the event; End counts symbols
  through its end. The Sequence column prints the complete inserted or deleted sequence.
  See <https://github.com/bredelings/BAli-Phy/blob/master/tests/tools/joint-indels/README.md> for output format documentation.


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

