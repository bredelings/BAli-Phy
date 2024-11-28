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

**-V**, **--verbose**
: Output more log messages on stderr.


# SEQUENCE FILTERING OPTIONS:
**-p** _arg_, **--protect** _arg_
: Sequences that cannot be removed (comma-separated).

**-k** _arg_, **--keep** _arg_
: Remove sequences not in comma-separated list _arg_.

**-r** _arg_, **--remove** _arg_
: Remove sequences in comma-separated list _arg_.

**-l** _arg_, **--longer-than** _arg_
: Remove sequences not longer than _arg_.

**-s** _arg_, **--shorter-than** _arg_
: Remove sequences not shorter than _arg_.

**-c** _arg_, **--cutoff** _arg_
: Remove similar sequences with #mismatches < cutoff.

**-d** _arg_, **--down-to** _arg_
: Remove similar sequences down to _arg_ sequences.

**--remove-gappy** _arg_
: Remove _arg_ outlier sequences -- defined as sequences that are missing too many conserved sites.

**--conserved** _arg_ (=0.75)
: Fraction of sequences that must contain a letter for it to be considered conserved.


# COLUMN FILTERING OPTIONS:
**-K** _arg_, **--keep-columns** _arg_
: Keep columns from this sequence

**-m** _arg_, **--min-letters** _arg_
: Remove columns with fewer than _arg_ letters.

**-u** _arg_, **--remove-unique** _arg_
: Remove insertions in a single sequence if longer than _arg_ letters

**-e**, **--erase-empty-columns**
: Remove columns with no characters (all gaps).


# OUTPUT OPTIONS:
**-S**, **--sort**
: Sort partially ordered columns to group similar gaps.

**-L**, **--show-lengths**
: Just print out sequence lengths.

**-N**, **--show-names**
: Just print out sequence lengths.

**-F** _arg_, **--find-dups** _arg_
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

```
% alignment-thin --keep=seq1,seq2   file.fasta > file2.fasta
```

Remove short sequences:
```
% alignment-thin --longer-than=250 file.fasta > file-long.fasta
```

Remove similar sequences with <= 5 differences from the closest other sequence:
```
% alignment-thin --cutoff=5 file.fasta > more-than-5-differences.fasta
```

Remove similar sequences until we have the right number of sequences:
```
% alignment-thin --down-to=30 file.fasta > file-30taxa.fasta
```

Remove dissimilar sequences that are missing conserved columns:
```
% alignment-thin --remove-gappy=10 file.fasta > file2.fasta
```

Protect some sequences from being removed:
```
% alignment-thin --down-to=30 file.fasta --protect=seq1,seq2 > file2.fasta
```

```
% alignment-thin --down-to=30 file.fasta --protect=@filename > file2.fasta
```


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

