% model_P(1)
% Benjamin Redelings
% Feb 2018

# NAME

**model_P** - Compute marginal likelihoods using stabilized harmonic mean estimator

# SYNOPSIS

**model_P** [OPTIONS] < in-file

# DESCRIPTION

Compute marginal likelihoods using the stabilized harmonic mean estimator.
Read whitespace-separated natural-log likelihoods from standard input. The estimate
and its bootstrap standard deviation are reported on the natural-log scale.

# ALLOWED OPTIONS:
**-h**, **--help**
: produce help message

**--seed** _arg_
: random seed

**--verbose**
: Output more log messages on stderr.


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

