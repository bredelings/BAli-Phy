% joint-parsimony(1)
% Benjamin Redelings
% Feb 2018

# NAME

**joint-parsimony** - Usage: joint-parsimony <alignments file> <trees file> [OPTIONS]

# SYNOPSIS

**joint-parsimony** _ALIGNMENTS_ _TREES_ [OPTIONS]

# DESCRIPTION

_ALIGNMENTS_ and _TREES_ are required positional filenames containing alignment
samples and corresponding tree samples, respectively.

Indel lengths count alphabet symbols, so a codon counts as one symbol.
For DNA/RNA and nucleotide triplet/codon alphabets, phase counts classify indels
by the two ungapped sequence positions modulo three, measured in nucleotides.
An event is counted in `phase0`, `phase1`, or `phase2` when those positions agree
modulo three, or in `no-phase` otherwise. Reading-frame interpretation assumes
coding sequences beginning at a codon boundary. Other alphabets omit these fields.

# ALL OPTIONS:
**-h**, **--help**
: produces help message

**--subsample** _arg_ (=10)
: factor by which to sub-sample trees

**--alphabet** _arg_
: set to 'Codons' to prefer codon alphabets


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

