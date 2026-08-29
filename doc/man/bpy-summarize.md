% bpy-summarize(1)
% Benjamin Redelings
% Feb 2018

# NAME

**bpy-summarize** - Generate an HTML report summarizing bali-phy runs.

# SYNOPSIS

**bpy-summarize** [OPTIONS] _directory1_ [_directory2_ ... ]

# DESCRIPTION

Generate an HTML report summarizing bali-phy runs.

When every chain contains a `C1.PN.site-property-samples.jsonl` stream for a partition,
**bpy-summarize** computes pooled posterior means, standard deviations, and
medians for observed-character properties. It embeds an interactive property
viewer and ranked template-column reports in that partition's tip-alignment
pages. The partition alphabet is used to tokenize logical characters, color
compound symbols, and translate codons.

When a partition contains a property ending in `posSelection`, the main report
also contains a positive-selection section. It reports posterior support for
positive selection, links each support statistic to every partition using the
corresponding substitution model, and shows the ten highest-ranked selected
columns for each partition and property. The displayed columns match the
positive-selection text report from **character-properties**. A complete TSV
table and the alignment viewer are linked from each subsection.

The complete scalar-name mapping in `C1.log.column-map.json` allows the report
to associate the short names in `C1.log` with substitution models and
partitions.

Partitions with property logs missing from one or more chains are skipped
rather than pooling a biased subset. Property overlays are omitted from
ancestral and alignment-difference pages.

The report is created at **Results/index.html**.

Complete positive-selection tables are written as
`Results/P1.positive-selection.tsv`. Prefixed properties use corresponding
names such as `Results/P1.foreground-positive-selection.tsv`.

A log of all sub-commands is created at **Results/commands.log**.

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
See **Results/commands.log** to check if any sub-commands failed.

 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
