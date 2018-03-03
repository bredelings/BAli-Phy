% path-graph(1)
% Benjamin Redelings
% Feb 2018

# NAME

**path-graph** -- Compute a consensus alignment for the alignments given.

# SYNOPSIS

**path-graph** [OPTIONS] < alignments-file

# DESCRIPTION

Compute a consensus alignment for the alignments given.

# ALLOWED OPTIONS:
**-h**, **--help**
: produce help message

**--alphabet** _arg_
: Specify the alphabet: DNA, RNA, Amino-Acids, Amino-Acids+stop, Triplets, Codons, or Codons+stop.

**--taxa** _arg_
: Colon-separate pair of taxon names

**--skip** _arg_ (=0)
: number of tree samples to skip

**--max-alignments** _arg_ (=1000)
: maximum number of alignments to analyze


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

