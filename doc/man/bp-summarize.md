% bp-summarize(1)
% Benjamin Redelings
% Feb 2018

# NAME

**bp-summarize** - Generate an HTML report summarizing bali-phy runs.

# SYNOPSIS

**bp-summarize** [OPTIONS] _directory1_ [_directory2_ ... ]

# DESCRIPTION

Generate an HTML report summarizing bali-phy runs.

When every chain contains a `C1.propertiesN.json` stream for a partition,
**bp-summarize** computes pooled posterior means, standard deviations, and
medians for observed-character properties. It embeds an interactive property
viewer and ranked template-column reports in that partition's tip-alignment
pages. The partition alphabet is used to tokenize logical characters, color
compound symbols, and translate codons.

Partitions with property logs missing from one or more chains are skipped
rather than pooling a biased subset. Property overlays are omitted from
ancestral and alignment-difference pages.

The report is created at **Results/index.html**.

A log of all sub-commands is created at **Results/bp-summarize.log**.

# OPTIONS:
**-h**, **--help**
: Print usage information.

**--skip=NUM**
: Discard alignment and character-property records at or before iteration NUM

**--subsample=NUM**
: Keep only every NUMth stored sample. Alignment and character-property
  samples are normally stored every 10 MCMC iterations, so this applies
  additional thinning to those streams.


# REPORTING BUGS:
See **Results/bp-summarize.log** to check if any sub-commands failed.

 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
