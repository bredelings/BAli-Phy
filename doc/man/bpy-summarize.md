% bpy-summarize(1)
% Benjamin Redelings
% Feb 2018

# NAME

**bpy-summarize** - Generate an HTML report summarizing bali-phy runs.

# SYNOPSIS

**bpy-summarize** [OPTIONS] _directory1_ [_directory2_ ... ]

# DESCRIPTION

For `BranchModel`, the Branch models section summarizes category ω values. For `BranchModel_test`,
it also reports Rao–Blackwellized hypothesis support and ω summaries conditional on each hypothesis.
Conditional tables include matching-sample counts, but no autocorrelation or convergence diagnostics;
the ordinary scalar table retains diagnostics on unfiltered parameter values.

The tables use the ordinary logged `omegas` and `hypothesis` fields. When a model uses
`get_state(branch_category_vectors)`, the generated analysis also writes
`C1.branch-category-usage.json` once. Matching metadata across chains lets the report omit unused
categories from each conditional table. Explicit `branchCats` may lack that metadata; in that case
all logged categories are shown with a warning. Ordinary `BranchModel` does not need usage metadata.

Under the default independent priors, an unused ω retains its prior distribution conditional on
that hypothesis. Unconditional category summaries therefore include prior draws from hypotheses
where the category is unused. Category properties describe model categories, not variation among
sites; branch-model category properties are not presented as site-selection tables.

Fixed or externally defined arguments may not have these model-local fields in the log. The report
explains which estimates are unavailable and retains available hypothesis support and ordinary
scalar summaries. Branch-specific summaries and tree coloring are not provided by this section.

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
corresponding substitution model, and shows up to 20 selected columns ordered by
overall posterior probability. The posterior with selection assumes positive
selection is included in the model. A complete TSV table and the alignment
viewer are linked from each subsection. When a model has no variable selector
and its selection status is fixed, the report states whether selection is included.

The complete scalar-name mapping in `C1.log.column-map.json` allows the report
to associate the short names in `C1.log` with substitution models and
partitions.

Partitions with property logs missing from one or more chains are skipped
rather than pooling a biased subset. Property overlays are omitted from
ancestral and alignment-difference pages.

The report is created at **Results/index.html**.

Complete positive-selection tables are written as
`Results/P1.positive-selection.tsv`. Prefixed properties use corresponding
names such as `Results/P1.foreground-positive-selection.tsv`. Each table gives
the model-averaged probability and dN/dS summaries first, followed by the
corresponding values conditioned on `positiveSelectionInModel`. Conditioned
fields are empty when no conditioned posterior is available, and dN/dS fields
are empty when the corresponding property was not recorded.

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
