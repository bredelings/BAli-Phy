% bp-analyze(1)
% Benjamin Redelings
% Feb 2018

# NAME

**bp-analyze** - Generate an HTML report summarizing bali-phy runs.

# SYNOPSIS

**bp-analyze** [OPTIONS] _directory1_ [_directory2_ ... ]

# DESCRIPTION

Generate an HTML report summarizing bali-phy runs.

The report is created at **Reports/index.html**.

A log of all sub-commands is created at **Reports/bp-analyze.log**.

# OPTIONS:
**-h**, **--help**
: Print usage information.

**--skip=NUM**
: Skip NUM iterations as burnin

**--subsample=NUM**
: Keep only every NUMth stored sample. Alignment and character-property
  samples are normally stored every 10 MCMC iterations, so this applies
  additional thinning to those streams.


# REPORTING BUGS:
See **Reports/bp-analyze.log** to check if any sub-commands failed.

 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
