% trees-bootstrap(1)
% Benjamin Redelings
% Feb 2018

# NAME

**trees-bootstrap** - Select only partitions with support in the specified range.

# SYNOPSIS

**trees-bootstrap** _partitions-file_ _trees-file_ [OPTIONS]

# DESCRIPTION

Select only partitions with support in the specified range.

# INPUT OPTIONS:
**-h**, **--help**
: produce help message

**--skip** _arg_ (=0)
: Number of trees to skip.

**--until** _arg_
: Read until this number of trees.

**--max** _arg_
: Thin down to this number of trees.

**--subsample** _arg_
: Factor by which to sub-sample.

**--verbose**
: Output more log messages on stderr.


# REPORTING OPTIONS:
**-n**, **--not**
: invert the results

**-b** _arg_, **--below** _arg_
: only report partitions with PP < arg

**-a** _arg_, **--above** _arg_
: only report partitions with PP > arg


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

