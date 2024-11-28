% alignment-consensus(1)
% Benjamin Redelings
% Feb 2018

# NAME

**alignment-consensus** - Construct a consensus alignment to summarize an alignment sample.

# SYNOPSIS

**alignment-consensus** alignments-file [alignments-file ...] [OPTIONS]

# DESCRIPTION

Construct a consensus alignment to summarize an alignment sample.

# ALLOWED OPTIONS:
**-h**, **--help**
: produce help message

**--alphabet** _arg_
: Specify the alphabet: DNA, RNA, Amino-Acids, Amino-Acids+stop, Triplets, Codons, or Codons+stop.

**-s** _arg_ (=0), **--skip** _arg_ (=0)
: number of alignment samples to skip

**-m** _arg_ (=1000), **--max** _arg_ (=1000)
: maximum number of alignments to analyze

**--strict** _arg_
: ignore events below this probability

**--cutoff** _arg_
: ignore events below this probability

**--uncertainty** _arg_
: file-name for AU uncertainty vs level

**--chop-to** _arg_
: keep only the first arg taxa

**-V**, **--verbose**
: Output more log messages on stderr.


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

