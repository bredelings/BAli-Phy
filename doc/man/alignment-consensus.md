% alignment-consensus(1)
% Benjamin Redelings
% Feb 2018

# NAME

**alignment-consensus** - Construct a consensus alignment to summarize an alignment sample.

# SYNOPSIS

**alignment-consensus** [OPTIONS] < alignments-file

# DESCRIPTION

Construct a consensus alignment to summarize an alignment sample.

# ALLOWED OPTIONS:
**-h**, **--help**
: produce help message

**--alphabet** _arg_
: Specify the alphabet: DNA, RNA, Amino-Acids, Amino-Acids+stop, Triplets, Codons, or Codons+stop.

**--skip** _arg_ (=0)
: number of tree samples to skip

**--max-alignments** _arg_ (=1000)
: maximum number of alignments to analyze

**--strict** _arg_
: ignore events below this probability

**--cutoff** _arg_
: ignore events below this probability

**--uncertainty** _arg_
: file-name for AU uncertainty vs level

**--verbose**
: Output more log messages on stderr.


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

