% alignment-distances(1)
% Benjamin Redelings
% Feb 2018

# NAME

**alignment-distances** - Compute distances between alignments.

# SYNOPSIS

**alignment-distances** _analysis_ alignments-file1 [alignments-file2 ...]

# DESCRIPTION

Compute distances between alignments.

# INPUT OPTIONS:
**-h**, **--help**
: Produce help message

**-s** _arg_ (=0), **--skip** _arg_ (=0)
: Number of alignment samples to skip.

**-m** _arg_ (=1000), **--max** _arg_ (=1000)
: Maximum number of alignments to analyze.

**-V**, **--verbose**
: Output more log messages on stderr.

**--alphabet** _arg_
: Specify the alphabet: DNA, RNA, Amino-Acids, Amino-Acids+stop, Triplets, Codons, or Codons+stop.


# ANALYSIS OPTIONS:
**--distances** _arg_
: (=splits:splits2:nonrecall:inaccuracy) Colon-separated list of distances.

**--analysis** _arg_
: Analysis: score, AxA, NxN, compare, median, distances

**--CI** _arg_ (=0.94999999999999996)
: Confidence interval size.

**--mean**
: Show mean and standard deviation

**--median**
: Show median and confidence interval

**--minmax**
: Show minimum and maximum distances


# EXAMPLES:
 
Compute distances from true.fasta to each in As.fasta:
```
% alignment-distances score true.fasta As.fasta
```

Compute distance matrix between all pairs of alignments in all files:
```
% alignment-distances AxA file1.fasta ... fileN.fasta
```

Compute all NxN pairwise alignment accuracies, averaged over As:
```
% alignment-distances NxN true.fasta As.fasta
```

Find alignment with smallest average distance to other alignments:
```
% alignment-distances median As.fasta A.fasta
```

Compare the distances with-in and between the two groups:
```
% alignment-distances compare A-dist1.fasta A-dist2.fasta
```

Report distribution of average distance to other alignments:
```
% alignment-distances distances As.fasta A.fasta
```


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

