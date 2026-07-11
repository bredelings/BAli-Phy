# Improving `bp-analyze`

The most useful way to think about `bp-analyze` is as three things currently fused together:

1. An importer for several MCMC output formats.
2. A scientific-analysis pipeline.
3. A report generator.

Each of those suggests a separate family of improvements. A fourth family—execution, reproducibility, and maintainability—cuts across all three.

## Relevant existing tools

### General MCMC summarization

- **ArviZ** is probably the closest Python analogue. It provides a common posterior-data model, numerical summaries, diagnostics, model comparison, and more than 30 plots. Its plotting layer supports Matplotlib, Bokeh, and Plotly backends. [ArviZ documentation](https://python.arviz.org/en/stable/index.html)

- The R **posterior** package supplies standardized draw containers and modern diagnostics such as rank-normalized R-hat, bulk ESS, tail ESS, and Monte Carlo standard errors. [posterior package](https://stat.ethz.ch/CRAN/web/packages/posterior/index.html)

- **bayesplot** adds trace, rank, autocorrelation, density, interval, pairs, and diagnostic plots, while accepting draws from many different MCMC programs. [bayesplot reference](https://mc-stan.org/bayesplot/reference/index.html)

- **ShinyStan** is particularly relevant to the “make it a web application” idea: it provides interactive tables and plots for exploring posterior samples and diagnosing MCMC behavior. [ShinyStan](https://mc-stan.org/shinystan/)

- **coda** is the older, widely used R framework for generic MCMC objects and diagnostics. It remains important for interoperability, although `posterior` represents a more modern diagnostic baseline.

### Phylogenetic MCMC tools

- **Tracer** supports scalar trace files from BEAST, BEAST2, MrBayes, RevBayes, and other programs. It adds trace inspection, density estimation, conditional summaries, demographic reconstructions, and multivariate visualization. [Tracer documentation](https://beast.community/tracer)

- **RWTY** is the closest comparison for tree-space convergence. It includes topology traces, tree autocorrelation, approximate topological ESS, sliding and cumulative split-frequency plots, ASDSF through time, correlations of split frequencies between chains, and MDS views of tree space. [RWTY analysis interface](https://rdrr.io/cran/rwty/man/analyze.rwty.html)

- **RevGadgets** reads RevBayes parameter and tree traces and produces parameter and publication-quality phylogenetic visualizations. It can hand scalar traces to `coda`. [RevGadgets tutorial](https://revbayes.github.io/tutorials/intro/revgadgets.html)

- **PhyloBayes** provides `tracecomp` for continuous summaries and `bpcomp` for bipartition-frequency agreement and pooled consensus trees. [PhyloBayes-MPI manual](https://bioweb.pasteur.fr/docs/modules/phylobayes/mpi-1.5a/pb_mpiManual1.5.pdf)

- **MrBayes** has built-in `sump` and `sumt` facilities for scalar summaries, consensus trees, ESS, PSRF, and ASDSF. [MrBayes manual](https://gensoft.pasteur.fr/docs/mrbayes/3.2.7/Manual_MrBayes_v3.2.pdf)

- The BEAST ecosystem separates responsibilities among **LogCombiner**, **TreeAnnotator**, tree viewers such as DensiTree/PearTree, and Tracer. [BEAST programs](https://beast.community/programs)

- For Python-native tree processing, **phylotreelib** is worth evaluating. It reads Newick/NEXUS, computes tree distances, and builds consensus, MCC, and related posterior summary trees. It is newer and should be assessed for maturity before becoming a core dependency. [phylotreelib](https://github.com/agormp/phylotreelib)

The important observation is that none of these completely subsumes `bp-analyze`. Its joint handling of scalar parameters, trees, alignments, ancestral sequences, BAli-Phy models, and source code is unusually broad.

## Improvement tracks worth considering separately

| Track | Central question | Examples |
|---|---|---|
| 1. Correctness and robustness | Does the current analysis reliably do what it claims? | Fix stale PhyloBayes/BEAST paths, honor `--outdir` everywhere, implement/remove `--clean`, validate inputs |
| 2. Program architecture | Can parts be changed without destabilizing everything else? | Separate importing, analysis, execution, artifact management, and presentation |
| 3. Common data model | What is a run, chain, trace, tree sample, alignment sample, and result? | Typed internal objects; JSON schemas; explicit chain and sample coordinates |
| 4. Format interoperability | Which external programs can be analyzed correctly? | BAli-Phy, BEAST/BEAST2, MrBayes, RevBayes, PhyloBayes adapters |
| 5. Scalar diagnostics | Are numerical chains assessed using current methods? | Rank-normalized R-hat, bulk/tail ESS, MCSE, rank plots, autocorrelation |
| 6. Tree-space diagnostics | Are topology convergence and uncertainty adequately characterized? | Sliding split frequencies, topology traces, tree ESS, ASDSF through time |
| 7. Alignment-specific analysis | What can `bp-analyze` uniquely offer for BAli-Phy? | Alignment uncertainty, posterior homology, indel summaries, ancestral-state uncertainty |
| 8. Interactive report | How should users explore results? | Filtering, linked plots, expandable trees, parameter search, warnings dashboard |
| 9. Workflow and caching | How should expensive results be computed and reused? | Dependency graph, fingerprints, parallel jobs, resumable and partial reports |
| 10. Reproducibility | Can a report be audited or regenerated? | Versions, commands, checksums, environment, options, structured provenance |
| 11. Extensibility | How can new analyses be added? | Adapter and analysis plugin interfaces; machine-readable result API |
| 12. Testing and packaging | Can it evolve safely and install cleanly? | Fixture runs, golden reports, unit tests, dependency checks, CI |
| 13. Performance and scale | Can it handle very large traces and tree sets? | Streaming readers, bounded sampling, incremental summaries, memory budgets |
| 14. Scientific interpretation | Does it help users decide what results mean? | Explicit diagnostic statuses, explanations, thresholds, sensitivity analyses |

### 1. Correctness and robustness

This should be considered independently from larger redesigns because it produces immediate value.

The current script contains recognizable unfinished behavior: `--clean` is unused, some paths hardcode `Results`, and the PhyloBayes/BEAST classes are not presently usable as written. Input-type inference also conflates recognition, parsing, and error reporting.

A focused stabilization pass could provide:

- Strict option validation: positive subsampling, nonnegative burn-in, `until > skip`.
- Clear recognition diagnostics explaining why an input failed.
- Preflight checking of every required external program.
- Partial reports when optional analyses fail.
- Proper HTML escaping and offline-safe report assets.
- A machine-readable error/status file.
- Consistent handling of output directories.

This work should precede new scientific functionality.

### 2. Program structure and internal data model

At present, constructing `Analysis` immediately runs the entire pipeline. That makes parsing, computation, testing, and rendering difficult to exercise separately.

A stronger design would have layers such as:

```text
Run adapters -> normalized analysis dataset -> analysis tasks -> result objects -> renderers
```

The normalized dataset is the critical piece. It might contain:

- Chains and sample indices.
- Scalar variables with names, dimensions, and metadata.
- Tree samples and taxa.
- Alignment samples by partition.
- Model descriptions and input files.
- Provenance and program-specific metadata.

Then an analysis produces structured results—rather than HTML fragments or implicitly named files. Both a static HTML renderer and a future web application could consume the same result objects.

### 3. Import adapters and cross-program support

Supporting other phylogenetic programs should be an adapter problem, not a series of special cases inside the analysis.

Each adapter should answer:

- Does this input belong to me?
- Which files form each chain?
- What is the sample number/generation column?
- Which scalar, tree, and alignment capabilities exist?
- What burn-in and thinning units are used?
- What model and provenance metadata are available?

Separate adapters could cover:

- BAli-Phy 4 JSON output.
- Legacy BAli-Phy output.
- BEAST/BEAST2 log and NEXUS tree files.
- MrBayes `.p`/`.t` files.
- RevBayes logs and tree traces.
- PhyloBayes `.trace`/`.treelist` prefixes.
- Generic tabular logs and Newick/NEXUS tree streams.

It would also be useful to distinguish “input compatibility” from “native semantic support.” A generic BEAST log may be readable without understanding skyline parameters, for example.

### 4. Modern scalar diagnostics

The script currently parses the textual output of `statreport`. It would be worth deciding whether `statreport` should remain authoritative or whether scalar data should additionally enter ArviZ or R `posterior`.

The modern baseline would include:

- Rank-normalized, split, and folded R-hat.
- Bulk and tail ESS.
- MCSE for means, standard deviations, and important quantiles.
- Rank histograms or rank ECDF plots.
- Trace and density overlays by chain.
- Autocorrelation plots.
- Detection of constant or non-finite chains.
- Parameter filtering and grouping.

The `posterior` package explicitly distinguishes tail ESS because adequate central sampling does not imply adequate tail sampling. [Tail ESS documentation](https://mc-stan.org/posterior/reference/ess_tail.html)

Existing BAli-Phy diagnostics need not be discarded. They could be reported alongside modern diagnostics during a validation period.

### 5. Better tree-space diagnostics

This is probably the highest-value scientific expansion.

`bp-analyze` already has consensus trees, split diagnostics, SRQ plots, and MDS. RWTY suggests several complementary views:

- Split frequencies in sliding windows.
- Cumulative split-frequency stabilization.
- ASDSF as a function of retained sample count.
- Per-chain topology traces relative to one or several reference trees.
- Tree-distance autocorrelation by lag.
- Approximate and pseudo-ESS for topology.
- Pairwise split-frequency scatterplots.
- Clustering chains based on split-frequency disagreement.
- Tree-space plots colored by chain, time, likelihood, or another scalar.

RWTY’s main interface produces most of these as a coherent suite. [RWTY outputs](https://rdrr.io/cran/rwty/f/README.md)

The larger design question is whether to invoke RWTY, port selected methods, or implement equivalent methods in BAli-Phy’s existing C++ utilities. That deserves its own investigation because correctness, performance, licensing, and maintenance differ substantially among those choices.

### 6. Treat burn-in as an analysis, not just an option

The current default is 10% of the shortest chain, unless chain lengths differ greatly. That is convenient but scientifically weak as a general rule.

Possible improvements include:

- Show diagnostics both before and after the proposed burn-in.
- Plot cumulative estimates and split frequencies.
- Permit burn-in as a fraction, iteration count, or generation.
- Offer a recommended value while clearly labeling it heuristic.
- Provide sensitivity comparisons across several burn-in choices.
- Record whether the user accepted an automatic recommendation.

Automatic burn-in should not conceal nonstationarity.

### 7. Expand the report into a static interactive application

A static, self-contained application is a useful step before introducing a server. It preserves the existing “copy a report directory anywhere” workflow while allowing substantial interactivity.

Useful features include:

- Searchable and sortable parameter table.
- Click a parameter to open its trace, density, rank, autocorrelation, and chain statistics.
- Cross-highlighting among trace, tree-space, and likelihood plots.
- Interactive tree zooming, collapsing, rerooting, and support filtering.
- Switching among consensus thresholds.
- Filtering splits by posterior probability or between-chain disagreement.
- Interactive alignment inspection and uncertainty overlays.
- A top-level traffic-light diagnostic dashboard, with every warning linked to its evidence.
- Download buttons for the data underlying every plot.
- Client-side selection of burn-in and subsampling for lightweight recalculations.

An embeddable tree component such as the emerging **heat-tree** library supports collapsing, metadata-driven styling, responsive layouts, and SVG/PNG export. [heat-tree documentation](https://grunwaldlab.github.io/heat-tree/) Plotly or Bokeh could handle linked statistical plots.

The backend should first emit clean JSON. Otherwise, more JavaScript would merely deepen the coupling to the current HTML generator.

### 8. Reporting and scientific interpretation

A report should distinguish:

- “The computation succeeded.”
- “The diagnostic crossed a conventional threshold.”
- “The run is demonstrably converged.”

The last is generally not something diagnostics can guarantee; RWTY explicitly notes that its plots provide necessary checks rather than proof of convergence. [RWTY paper](https://academic.oup.com/mbe/article/34/4/1016/2900564)

A stronger report could therefore present:

- Status: pass, warning, fail, unavailable.
- Observed value and threshold.
- Why the metric matters.
- What remedial action might help.
- Whether the metric requires multiple chains.
- Whether it applies to scalars, topology, alignments, or all three.
- Conflicts such as good scalar ESS but poor tree mixing.

This interpretive layer is distinct from both computation and presentation.

### 9. Workflow, caching, and provenance

The current timestamp-based reuse and `properties.json` logic is a useful beginning, but it can miss changes in tool versions or command semantics.

A proper task model could fingerprint:

- Input file size, timestamp, and optionally content hash.
- Analysis options.
- `bp-analyze` version.
- External program versions.
- Analysis-task version.

Each task would declare inputs, outputs, and dependencies. That enables:

- Parallel execution.
- Reliable incremental rebuilds.
- Resume after failure.
- `--force TASK`.
- `--only scalar,trees`.
- A report that remains usable when one optional task fails.
- An explicit manifest of every generated artifact.

### 10. Preserve and deepen BAli-Phy-specific strengths

The general packages mostly stop at scalar traces and tree samples. `bp-analyze` also understands sampled alignments, indel models, ancestral states, and BAli-Phy model code. That is its comparative advantage.

Separate improvement programs could explore:

- Pairwise homology posterior probabilities.
- Column and region-level alignment uncertainty.
- Alignment-length and gap-pattern traces.
- Correlation between topology and alignment uncertainty.
- Indel-event summaries.
- Partition-specific convergence.
- Ancestral-state uncertainty mapped onto the tree.
- Comparisons among posterior-decoding scoring rules.
- Posterior predictive checks for sequence, alignment, and tree statistics.

This is likely more valuable scientifically than merely reproducing every plot in Tracer.

## Suggested ordering

The work can be organized into four relatively independent programs:

1. **Foundation:** correctness, tests, structured internal data, adapters, and artifact manifests.
2. **Scientific diagnostics:** modern scalar diagnostics and expanded tree-space diagnostics.
3. **Interactive reporting:** JSON result format followed by a static interactive application.
4. **Domain expansion:** deeper alignment/indel analyses, posterior predictive checks, and robust support for other phylogenetic programs.

The architectural work should enable the others, but the scientific and interface tracks can then proceed independently. The goal should not be framed as “replace the script with ArviZ” or “turn it into ShinyStan.” A better goal is to reuse their mature ideas and implementations for scalar posterior analysis while retaining `bp-analyze` as an integrated, phylogenetics-aware report system.
