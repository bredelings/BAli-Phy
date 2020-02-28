% stats-cat(1)
% Benjamin Redelings
% Feb 2018

# NAME

**stats-cat** - Append tab-delimited files with the same field names.

# SYNOPSIS

**stats-cat** [OPTIONS] file1 [file2 file3 ... ]

# DESCRIPTION

Append tab-delimited files with the same field names.

# ALL OPTIONS:
**-h**, **--help**
: Produce help message.

**-v**, **--verbose**
: Output more log messages on stderr.

**-s** _arg_, **--skip** _arg_
: Number of initial lines to skip.

**-x** _arg_ (=1), **--subsample** _arg_ (=1)
: Factor by which to sub-sample.

**-u** _arg_, **--until** _arg_
: Read up to this iteration.

**-I** _arg_, **--ignore** _arg_
: Do not analyze these fields.

**-S** _arg_, **--select** _arg_
: Analyze only these fields.

**-O** _arg_, **--output** _arg_
: Output format: json, tsv

**--unnest**
: Unnest JSON file.


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

