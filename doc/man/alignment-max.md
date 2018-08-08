% alignment-max(1)
% Benjamin Redelings
% Feb 2018

# NAME

**alignment-max** - Construct a posterior decoding alignment to summarize an alignment sample.

# SYNOPSIS

**alignment-max** [OPTIONS] < alignments-file

# DESCRIPTION

Construct a posterior decoding alignment to summarize an alignment sample.

# ALLOWED OPTIONS:
**-h**, **--help**
: Produce help message

**--alphabet** _arg_
: Specify the alphabet: DNA, RNA, Amino-Acids, Amino-Acids+stop, Triplets, Codons, or Codons+stop.

**-s** _arg_ (=0), **--skip** _arg_ (=0)
: Number of alignment samples to skip

**-m** _arg_ (=1000), **--max-alignments** _arg_ (=1000)
: Maximum number of alignments to analyze

**--analysis** _arg_ (=wsum)
: sum, wsum, multiply

**-o** _arg_ (=-), **--out** _arg_ (=-)
: Output file (defaults to stdout)

**-p** _arg_, **--out-probabilities** _arg_
: Output file for column probabilities, if specified

**-v**, **--verbose**
: Output more log messages on stderr.


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

