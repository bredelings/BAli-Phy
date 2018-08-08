% alignment-identity(1)
% Benjamin Redelings
% Feb 2018

# NAME

**alignment-identity** - Report statistics on pairwise identity.

# SYNOPSIS

**alignment-identity** [OPTIONS] < sequence-file

# DESCRIPTION

Report statistics on pairwise identity.

# ALLOWED OPTIONS:
**-h**, **--help**
: produce help message

**--alphabet** _arg_
: Specify the alphabet: DNA, RNA, Amino-Acids, Amino-Acids+stop, Triplets, Codons, or Codons+stop.

**--with-indels**
: Calculate percent-identity w/ indels

**--seed** _arg_
: random seed

**--skip** _arg_ (=0)
: number of tree samples to skip

**--max-alignments** _arg_ (=1000)
: maximum number of alignments to analyze

**--cutoff** _arg_ (=0.75)
: ignore events below this probability

**--identity** _arg_ (=0.40000000000000002)
: Find fraction of sequences that have this level of identity.

**--analysis** _arg_
: What analysis to do: default, matrix, nmatrix

**--strict**
: require all implied pairs pass the cutoff


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

