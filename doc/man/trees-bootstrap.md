% trees-bootstrap(1)
% Benjamin Redelings
% Feb 2018

# NAME

**trees-bootstrap** - Compare support for partitions between different files.

# SYNOPSIS

**trees-bootstrap** _file1_ [_file2_ ... ] --predicates _predicate file_ [OPTIONS]

# DESCRIPTION

Compare support for partitions between different files.

# INPUT OPTIONS:
**-h**, **--help**
: Produce help message.

**-s** _arg_ (=10%), **--skip** _arg_ (=10%)
: Number of trees to skip.

**-u** _arg_, **--until** _arg_
: Read until this number of trees.

**-m** _arg_, **--max** _arg_
: Thin tree samples down to this number of trees.

**-x** _arg_, **--subsample** _arg_
: Factor by which to sub-sample.

**--predicates** _arg_
: Predicates to examine.

**--min-support** _arg_ (=0.1)
: Minimum value of predicates to consider interesting..

**-V**, **--verbose**
: Output more log messages on stderr.


# BLOCK BOOTSTRAP OPTIONS:
**--bootstrap**
: Do block bootstrapping to get a CI on the posterior probabilities.

**--samples** _arg_ (=10000)
: Number of bootstrap samples.

**--pseudocount** _arg_ (=1)
: Extra 0/1 to add to bootstrap samples.

**--blocksize** _arg_
: Block size to use in block boostrap.

**--seed** _arg_
: Random seed.


# REPORTING OPTIONS:
**--separation** _arg_ (=0)
: Only report trees/partitions if they differ by this many LODs

**--confidence** _arg_ (=0.95)
: Width of confidence intervals

**--LOD-table** _arg_
: Write the partitions LOD10's to a file.


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

