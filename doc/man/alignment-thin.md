% alignment-thin(1)
% Benjamin Redelings
% Feb 2018

# NAME

**alignment-thin** - Remove sequences or columns from an alignment.

# SYNOPSIS

**alignment-thin** _alignment-file_ [OPTIONS]

# DESCRIPTION

Remove sequences or columns from an alignment.

# GENERAL OPTIONS:
**-h**, **--help**
: Print usage information.

**-v**, **--verbose**
: Output more log messages on stderr.


# SEQUENCE FILTERING OPTIONS:
**--protect** _arg_
: Sequences that cannot be removed (comma-separated).

**--remove** _arg_
: Remove sequences in comma-separated list _arg_.

**--longer-than** _arg_
: Remove sequences not longer than _arg_.

**--shorter-than** _arg_
: Remove sequences not shorter than _arg_.

**--cutoff** _arg_
: Remove similar sequences with #mismatches < cutoff.

**--down-to** _arg_
: Remove similar sequences down to _arg_ sequences.

**--remove-crazy** _arg_
: Remove _arg_ outlier sequences -- defined as sequences that are missing too many conserved sites.

**--conserved** _arg_ (=0.75)
: Fraction of sequences that must contain a letter for it to be considered conserved.


# COLUMN FILTERING OPTIONS:
**--min-letters** _arg_
: Remove columns with fewer than _arg_ letters.

**--remove-unique** _arg_
: Remove insertions in a single sequence if longer than _arg_ letters


# OUTPUT OPTIONS:
**--sort**
: Sort partially ordered columns to group similar gaps.

**--show-lengths**
: Just print out sequence lengths.

**--find-dups** _arg_
: For each sequence, find the closest other sequence.


# EXAMPLES:
 
Remove columns without a minimum number of letters:
```
% alignment-thin --min-letters=5 file.fasta > file-thinned.fasta
```

Remove sequences by name:
```
% alignment-thin --remove=seq1,seq2 file.fasta > file2.fasta
```

Remove short sequences:
```
% alignment-thin --longer-than=250 file.fasta > file-long.fasta
```

Remove sequences with <= 5 differences from the closest other sequence:
```
% alignment-thin --cutoff=5 file.fasta > more-than-5-differences.fasta
```

Like --cutoff, but stop when we have the right number of sequences:
```
% alignment-thin --down-to=30 file.fasta > file-30taxa.fasta
```

Remove dissimilar sequences that are missing conserved columns:
```
% alignment-thin --remove-crazy=10 file.fasta > file2.fasta
```

Protect some sequences from being removed:
```
% alignment-thin --down-to=30 file.fasta --protect=seq1,seq2 > file2.fasta
```


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

