% bpy-summarize(1)
% Benjamin Redelings
% Feb 2018

# NAME

**bpy-summarize** - Generate an HTML report summarizing BAli-Phy runs.

# SYNOPSIS

**bpy-summarize** \[OPTIONS\] _directory1_ [_directory2_ ... ]

# DESCRIPTION

**bpy-summarize** generates an HTML report from one or more BAli-Phy run directories.
The report summarizes sampled parameters, convergence diagnostics, trees, and alignments,
with additional summaries depending on the model and available output. When supplying multiple
run directories, use independent runs of the same analysis.

By default, the report is written to **Results/index.html**. Open this file in a web browser.
You can generate a report while runs are still in progress and rerun the command later to update it.

# EXAMPLES

Summarize one run:

    bpy-summarize run-1

Combine independent runs of the same analysis:

    bpy-summarize run-1 run-2

Discard the first 1000 iterations from each run as burn-in:

    bpy-summarize --skip=1000 run-1 run-2

# OPTIONS

**`-h`**, **`--help`**
: Print usage information.

**`--skip=NUM`**
: Discard the initial NUM iterations as burn-in. By default, use 10% of the shortest run.
  If the longest run is more than three times as long as the shortest, an explicit value is required.

**`--subsample=NUM`**
: Keep only every NUMth stored sample after burn-in. The default is 1 (no additional thinning).
  Alignment and character-property samples are normally stored every 10 MCMC iterations;
  NUM refers to stored samples, not iterations.

**`--until=NUM`**
: Limit the input used for summaries to NUM. For ordinary BAli-Phy output this is an iteration
  limit; tree-only input uses a tree count. By default, use all available input.

**`--prune=TAXA`**
: Remove the comma-separated list of taxa from tree summaries.

**`--outdir=DIRECTORY`**
: Write the report and supporting files to DIRECTORY instead of **Results**.

**`--verbose`**
: Print additional diagnostic information.

**`--sub-partitions`**
: Include partial splits and extended consensus trees in the tree summaries.

**`--node-bubbles`**
: Draw consensus trees with node bubbles representing branch length associated with
  unresolved relationships.

# SPECIALIZED SUMMARIES

## Character properties

When character properties were logged for a partition in every chain, the report includes
posterior means, standard deviations, and medians, along with an interactive viewer in the
partition's tip-alignment pages. Properties can be displayed as alignment colors to explore
variation among characters. Property summaries are omitted for a partition if any chain lacks
its property log.

## Positive selection

For models that log positive-selection properties, the report includes posterior support for
positive selection and ranked alignment columns, with links to the alignment viewer and complete
TSV tables.

Model-averaged estimates account for uncertainty about whether positive selection is included in
the model. Conditional estimates assume it is included. When selection status is fixed rather
than sampled, the report states whether selection is included.

## Branch models

For `BranchModel`, the report summarizes category ω values. For `BranchModel_test`, it also
reports hypothesis support and ω summaries conditional on each hypothesis. Conditional tables
include matching-sample counts but no autocorrelation or convergence diagnostics; the ordinary
parameter table provides diagnostics for the unfiltered samples.

Under the default independent priors, an unused ω retains its prior distribution conditional on
that hypothesis. Unconditional category summaries therefore include prior draws from hypotheses
where the category is unused. These categories describe branch models, not variation among sites.

# OUTPUT FILES

Paths below are relative to the output directory (**Results** by default).

**index.html**
: Main report, with links to supporting plots, trees, and alignments.

**P1.positive-selection.tsv**
: Complete positive-selection table for partition 1, with corresponding files for other partitions.
  Named properties use filenames such as **P1.foreground-positive-selection.tsv**. Tables give
  model-averaged probabilities and dN/dS summaries first, followed by conditional values.
  Conditional fields are empty when a conditional posterior is unavailable; dN/dS fields are
  empty when the corresponding property was not recorded.

**commands.log**
: Log of subcommands run to generate the report, useful for investigating failures.

# REPORTING BUGS

Check **commands.log** in the output directory for failed subcommands.

BAli-Phy online help: <https://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
