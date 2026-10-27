% path-graph(1)
% Benjamin Redelings
% Feb 2018

# NAME

**path-graph** - Count pairwise alignment-path edges across sampled alignments.

# SYNOPSIS

**path-graph** **`--taxa`** _names_ \[OPTIONS\] < alignments-file

# DESCRIPTION

Count pairwise alignment-path edges across sampled alignments of the same sequences.
Gap placement may vary, but the underlying sequences must match. For each requested
pair, report how often each path edge occurs among the retained alignments.

# ALLOWED OPTIONS:
**-h**, **--help**
: produce help message

**--alphabet** _arg_
: Specify the alphabet: DNA, RNA, Amino-Acids, Amino-Acids+stop, Triplets, Codons, or Codons+stop.

**--taxa** _arg_
: Colon-separated taxon names; report every pair. Required.

**--skip** _arg_ (=0)
: Number of alignment samples to skip (nonnegative).

**--max-alignments** _arg_ (=1000)
: Maximum number of retained alignments (positive, or -1 for no limit). The sample is
  thinned across the input rather than truncated to its first entries.


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

