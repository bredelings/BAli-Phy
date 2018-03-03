% joint-indels(1)
% Benjamin Redelings
% Feb 2018

# NAME

**joint-indels** -- Usage: joint-indels <alignments file> <trees file> [OPTIONS]

# SYNOPSIS

**joint-indels** _alignments file_ _trees file_ [OPTIONS]

# DESCRIPTION

Usage: joint-indels <alignments file> <trees file> [OPTIONS]

# ALLOWED OPTIONS:
**--help**
: produces help message

**--subsample** _arg_ (=10)
: factor by which to sub-sample trees

**--partition** _arg_
: find indels along internal branch that bi-partitions given taxa (<taxa1>:<taxa2>:...)

**--alphabet** _arg_
: set to 'Codons' to prefer codon alphabets

**--verbose**
: Output more log messages on stderr.


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

