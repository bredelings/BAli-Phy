% alignment-median(1)
% Benjamin Redelings
% Feb 2018

# NAME

**alignment-median** -- Don't use this program.  It doesn't work.

# SYNOPSIS

**alignment-median** [OPTIONS] < in-file

# DESCRIPTION

Don't use this program.  It doesn't work.

# INPUT OPTIONS:
**-h**, **--help**
: Produce help message

**-s** _arg_ (=0), **--skip** _arg_ (=0)
: number of alignment samples to skip

**-m** _arg_ (=1000), **--max** _arg_ (=1000)
: maximum number of alignments to analyze

**-v**, **--verbose**
: Output more log messages on stderr.

**--alphabet** _arg_
: Specify the alphabet: DNA, RNA, Amino-Acids, Amino-Acids+stop, Triplets, Codons, or Codons+stop.


# ANALYSIS OPTIONS:
**--metric** _arg_ (=splits)
: type of distance: pairs, splits, splits2

**--analysis** _arg_ (=matrix)
: Analysis: matrix, median, diameter

**--CI** _arg_ (=0.94999999999999996)
: Confidence interval size.

**--mean**
: Show mean and standard deviation

**--median**
: Show median and confidence interval

**--minmax**
: Show minumum and maximum distances


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

