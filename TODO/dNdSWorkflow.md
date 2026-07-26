# A Bayesian dN/dS and positive-selection workflow

## Scope

This note asks what BAli-Phy needs in order to provide a Bayesian counterpart to the branch, site, and
branch-site workflow in the PAML beginner's guide. It uses the following papers as its main points of
reference:

* Redelings (2014), [Erasing Errors due to Alignment Ambiguity When Estimating Positive
  Selection](https://pmc.ncbi.nlm.nih.gov/articles/PMC4155473/)
  ([doi:10.1093/molbev/msu174](https://doi.org/10.1093/molbev/msu174)).
* Alvarez-Carretero, Kapli, and Yang (2023), [Beginner's Guide on the Use of PAML to Detect Positive
  Selection](https://pmc.ncbi.nlm.nih.gov/articles/PMC10127084/)
  ([doi:10.1093/molbev/msad041](https://doi.org/10.1093/molbev/msad041)).

The goal is not to reproduce CODEML's likelihood-ratio tests or its text output. The goal is to answer
the same biological questions while retaining BAli-Phy's ability to integrate over parameter and
alignment uncertainty.

## Changes since the previous draft

Several parts of the previous survey and plan have been implemented or superseded:

* Context-dependent loggers now use `ContextAction`. `positiveSelectionFields` computes
  `LogOddsPosSelection` once and derives `PrPosSelection` from the same value, so the two fields agree
  within each logger invocation.
* The old `--Rao-Blackwellize` mechanism has been removed. `condLogOdds` and `condPr` use the same
  dimension-changing candidate construction as categorical Gibbs sampling.
* `branch_site` now represents every mixture component as a `BranchModel`. It preserves all
  properties of the input foreground and background models under `foreground-` and `background-`
  prefixes.
* `BranchModel` is active infrastructure used by `branch_site`; the previous proposal to delete it is
  obsolete.
* The special `MixtureModel` binding category and `mmm` representation have been removed.
* `calc-properties`, `alignment-draw`, and `bp-analyze` already provide a usable mean-property
  workflow. `alignment-draw` already has property selection, color scales, ranges, and tooltips.
* Tool tests are now declarative test directories enumerated separately by Meson. New `statreport` or
  `alignment-draw` cases should use that format instead of adding Python test methods or executables.
* The documentation already describes the fixed-topology requirement, model-indicator fields, and
  basic property display. The remaining documentation work is to connect these pieces into one
  scientifically explicit workflow and explain the more accurate model-probability summaries.

## Survey

### The PAML workflow

The PAML guide organizes the analysis around four increasingly heterogeneous codon models.

1. **M0, or one ratio.** One value of `omega = dN/dS` applies to every site and branch. This estimates
   the gene-wide average selective pressure and provides a baseline, but has little power to find
   positive selection affecting only a few sites or branches.
2. **Site models.** M1a versus M2a and M7 versus M8 test whether some sites have `omega > 1`. Under
   M2a or M8, BEB reports each site's posterior probability of belonging to a positive-selection
   class and its posterior mean and standard deviation of `omega`.
3. **Branch models.** A two-ratio model gives prespecified foreground branches a different `omega`
   from background branches and is compared with M0. This tests a difference between branch
   categories; it does not by itself prove that foreground `omega > 1`.
4. **Branch-site model A.** A null model in which the foreground's accelerated classes have
   `omega = 1` is compared with an alternative in which their `omega` may exceed one. BEB then
   identifies sites assigned to those classes on the prespecified foreground branches.

The operational workflow also includes preparing an in-frame codon alignment, choosing a genetic code
and codon-frequency model, supplying a tree, labeling foreground branches before looking at the test
result, checking multiple optimization starts, interpreting estimated parameters, and correcting for
multiple tests when many branches or genes are examined. The guide emphasizes that a very large branch
`omega` may merely reflect little or no synonymous change, and that alignment error, multinucleotide
changes, recombination, and biased gene conversion can create false positives.

### What the 2014 BAli-Phy work contributed

The 2014 work implemented a Bayesian branch-site test with a binary model indicator `H`:

```text
H = 0: foreground switching is retained, but the accelerated omega is 1
H = 1: the accelerated foreground omega is greater than 1
```

It used a prior probability of one half for each hypothesis. Consequently, in that particular model
the posterior odds equaled the Bayes factor. The paper also introduced a Rao-Blackwellized estimate:
at every MCMC iteration it computed `Pr(H = 1 | all other current variables)` and averaged those
conditional probabilities. This is more useful than counting sampled indicator values when the
posterior probability is close to zero or one.

The main scientific difference from CODEML was that the alignment was treated as an unobserved
variable. The test integrated over alignments rather than conditioning on one estimated alignment,
allowing the null and alternative models to favor different alignments. In the simulations this
removed the large false-positive excess caused by alignment error. The implementation still assumed a
fixed topology because the foreground branches had to be known and labeled in advance.

The paper did not provide the current site-reporting workflow. In particular, it did not provide a
maintained way to report posterior `dN/dS` or positive-selection values at observed characters, nor a
general context-dependent logger. Those limitations no longer describe the current implementation.

### Current BAli-Phy support

#### Models and evidence

* `m1a`, `m2a`, `m7`, `m8`, and `m8a` implement the corresponding site-model families.
* `m2a_test`, `m3_test`, and `m8a_test` use a model indicator to perform Bayesian site-model tests.
* `branch_site` implements the modified branch-site model with foreground branches taken from the
  tree's `foreground` edge attribute.
* `busted` and `busted_s` provide broader episodic-selection models, including a version with
  synonymous-rate variation.
* The maintained positive-selection tests use `positiveSelectionFields` to log
  `LogOddsPosSelection` and `PrPosSelection`.
* `ContextAction` identifies computations that are evaluated against an MCMC context. The logger
  prepares these fields outside changeable logger evaluation and then supplies the resulting object
  to the ordinary logger.
* `condLogOdds` and `condPr` use the corrected categorical-candidate algorithm, so they remain valid
  when changing the indicator creates or removes random variables.

#### Site and branch properties

* `dNdS` installs the state properties `dNdS` and `posSelection`. Mixture categories therefore carry
  their current `omega` and an indicator for `omega > 1`.
* `branch_site` prefixes every input background property with `background-` and every input
  foreground property with `foreground-`. This reuses the input model's property definitions instead
  of duplicating `dNdS` logic in the branch-site constructor.
* The outer `Discrete (BranchModel m)` retains the common property map needed by the existing sampled
  property machinery.
* With `--set write-properties=true`, BAli-Phy writes sampled category/state assignments and property
  tables. `calc-properties` accumulates posterior means at stable ungapped sequence-character
  coordinates.

#### Reporting

* `alignment-draw` displays property means, allows property and scale selection, supports robust and
  custom ranges, and includes property values in tooltips.
* `bp-analyze` discovers property streams, runs `calc-properties`, and incorporates the resulting
  alignment views into its report.
* Ordinary scalar logs and `bp-analyze` already provide parameter summaries, credible intervals,
  ESS-like diagnostics, and between-chain diagnostics.
* `statreport` can summarize ordinary and log-scale fields, but it cannot yet calculate the posterior
  probability represented by a trace of conditional log odds.

### Remaining gaps

* **Gene-wide M0:** `gy94` and related models provide the estimate, but it is not presented as part of
  one coherent selection workflow.
* **Site-model tests:** the inference and conditional model probabilities exist, but their evidence
  and site properties need a selection-specific report.
* **Two-ratio branch analysis:** `BranchModel` supplies the representation, but there is no maintained
  binding or Bayesian comparison corresponding to PAML's branch test.
* **Branch-site gene evidence:** `branch_site` and its conditional probability fields exist, but
  posterior odds and Bayes factors are not summarized correctly.
* **BEB-like site output:** posterior property means exist, but posterior SD, ranked sites, probability
  thresholds, and explicit reference-sequence coordinates do not.
* **Property semantics:** `alignment-draw` recognizes only the exact name `posSelection` as a
  probability. It therefore does not give `foreground-posSelection` the same default treatment.
* **Fixed foreground topology:** documentation requires a fixed topology, but generated models do not
  reject an unfixed topology when `branch_categories` is used.
* **Multiple genes or branch tests:** independent runs are possible, but prevalence-aware summaries
  and multiplicity guidance are absent.
* **Assumption checks:** models for multinucleotide changes and synonymous-rate variation exist, but
  the workflow does not organize sensitivity analyses around them.

## Statistical targets

### Gene-level evidence

For a model indicator `H`, the main estimate is

```text
p = E[Pr(H = 1 | the rest of the MCMC state) | data].
```

The sample mean of `PrPosSelection` estimates `p`. If the logged conditional log odds at iteration
`t` is `l[t]`, the numerically stable calculation is

```text
log posterior odds =
    logsumexp(log(sigmoid(l[t])))
  - logsumexp(log(sigmoid(-l[t]))).
```

The common sample-count term cancels. The arithmetic mean of `l[t]` is not the posterior log odds and
must not be reported as such. If the prior probability of `H = 1` is `pi`, then

```text
log BF10 = log posterior odds - log(pi / (1 - pi)).
```

The maintained positive-selection tests default to `pi = 0.5`, but a report must state the assumed
prior rather than silently treating posterior odds as a Bayes factor for every possible model.

`LogOddsPosSelection` and `PrPosSelection` agree when produced by one logger invocation because
`positiveSelectionFields` computes the conditional log odds once. TSV and JSON loggers still evaluate
context fields independently. If both formats are enabled, dimension-changing candidate values can
differ between the two evaluations even though each remains a valid conditional draw. The current
design accepts this because ordinary workflows use one format; changing random-stream ownership is a
separate logger design problem.

### Site-level evidence

For the current mixture models, each sampled site category has a `dNdS` property and a binary
`posSelection` property. Averaging the latter estimates

```text
Pr(the site is in an omega > 1 class | data).
```

For indicator tests this is an unconditional probability: a draw under `H = 0` contributes zero. This
includes uncertainty about whether the gene admits positive selection and is the most direct quantity
for reporting a site's overall evidence. A probability conditional on `H = 1` is a different
quantity. It may be useful later, but should not replace the unconditional probability or be
introduced without an explicit name.

The posterior mean and standard deviation of site `dN/dS` describe the posterior distribution of the
site's rate class. The standard deviation is posterior variation, not the Monte Carlo standard error
of the reported mean.

### Site coordinates under alignment uncertainty

An alignment column does not have a stable identity across MCMC samples. Observed sequence characters
do. The stored coordinate should therefore remain:

```text
(sequence name, zero-based ungapped model-character index)
```

A beginner-facing report may display that index as one-based and project it onto a selected alignment,
but the stored identity should not become the column number of a sampled or consensus alignment. A
selected reference sequence gives the closest analogue to CODEML's site number and reference amino
acid while retaining a meaningful identity when the alignment changes.

### Branch and branch-site evidence

A two-ratio branch model should separately report:

* evidence that foreground and background `omega` differ;
* the posterior distribution of each effective `omega`;
* `Pr(omega_foreground > 1 | data)`; and
* `Pr(omega_foreground > omega_background | data)`.

The first quantity is the Bayesian analogue of PAML's M0-versus-two-ratio comparison. It must not be
labeled as evidence for positive selection unless the foreground value is also above one.

For the branch-site model, the existing properties have explicit meanings:

* `background-dNdS` and `background-posSelection` come from the background model;
* `foreground-dNdS` and `foreground-posSelection` come from the foreground model.

The same mixture component index is used across branch categories, while the `BranchModel` chooses the
appropriate background or foreground matrix on each branch.

## Design investigation

### 1. Conditional model fields

**Choice:** Keep `ContextAction` and the current two-phase logger. Generalize
`positiveSelectionFields` into a small binary-indicator helper that computes conditional log odds once
and returns both the log odds and the probability. Define `positiveSelectionFields` in terms of it and
use it immediately for the branch comparison.

This is a missing part of the existing context-logger interface rather than a parallel framework. It
also keeps generated binding code from independently evaluating the two related fields.

**Alternatives:**

* Keeping only `positiveSelectionFields` would avoid one helper but duplicate the same calculation and
  naming convention for the branch test.
* A fully general context-dependent expression language could share arbitrary intermediate values,
  but no current workflow requires that infrastructure.
* Returning only log odds would be smaller, but ordinary logs and reports benefit from directly
  retaining the conditional probability too.

### 2. Evidence aggregation

**Choice:** Retain both the conditional probability and conditional log odds. Add a `statreport`
summary mode that interprets selected fields as conditional log odds and calculates the mean
conditional probability and resulting posterior log odds stably. `bp-analyze` should consume its
dedicated record through the existing `statreport` subprocess.

The calculation belongs in `statreport`, which already owns scalar trace reading, burn-in handling,
and sample selection. Reimplementing TSV, JSON, and MCON trace reading in `bp-analyze` would create
parallel infrastructure.

**Alternatives:**

* Averaging the sampled indicator is valid but can have much higher variance.
* Averaging the logged probability is simple but can lose probabilities near the floating-point
  limits.
* Averaging conditional log odds is mathematically wrong.
* Adding a selection-specific trace reader would make the command superficially independent but
  duplicate established parsing and filtering behavior.

### 3. Site identity and presentation

**Choice:** Keep ungapped sequence-character coordinates as the stored identity. Let
`alignment-draw` rank and filter sites for a selected sequence and show their positions in the
displayed alignment. For a codon alphabet, show both the reference codon and the amino acid obtained
from the configured genetic code.

**Alternatives:**

* Fixed alignment columns are familiar but cease to identify the same object when alignment is
  sampled.
* Consensus-alignment columns are useful presentation coordinates but depend on the consensus
  construction.
* A posterior homology-cluster identifier might be more symmetric than a reference sequence, but
  would be substantial new statistical and reporting infrastructure.

### 4. Site summaries

**Choice:** Extend `calc-properties` with a stable streaming second moment and output an optional `sd`
array alongside `mean` and `count`. Keep raw sampled property logs as the source data.

**Alternatives:**

* Retaining every value per character would permit arbitrary posterior quantiles, but its memory use
  grows with samples, properties, sequences, and sequence length.
* Rao-Blackwellizing every site-category assignment could reduce Monte Carlo variance, but would
  require conditional category probabilities from the phylogenetic likelihood and is not needed for
  the first usable workflow.

### 5. Property semantics

**Choice for the first workflow:** Treat `posSelection` and names ending in `-posSelection` as
probability properties in `alignment-draw`. Add a brief NOTE that this name-based rule should be
replaced if property results later carry explicit semantic metadata.

This is deliberately a narrow extension of the current exact-name rule. It correctly covers
`foreground-posSelection` and `background-posSelection` without changing the property representation
through the Haskell model, sampled stream, accumulator, JSON format, and viewer.

**Alternatives:**

* Adding a property-kind field to `StatePropertyMap` and preserving it through every output stage is
  clearer and more general, but is invasive relative to the two probability names currently needed.
* Treating every property whose values happen to lie in `[0,1]` as a probability would misclassify
  rates and other bounded quantities.
* Leaving only the exact `posSelection` check would give branch-site probabilities inappropriate
  default scales and thresholds.

### 6. Foreground and background models

**Choice:** Keep `BranchModel` as the common representation. Introduce one constructor in
`SModel.BranchSite` that combines a background model, a foreground model, and branch categories while
prefixing all properties from both inputs. Refactor `branchSite` to use it immediately, and use it
again in the next commit for the two-ratio branch model.

This preserves arbitrary input properties such as `dNdS`, `posSelection`, or future model-specific
properties. It avoids teaching the branch constructors how individual properties are computed.

**Alternatives:**

* Manually attaching `dNdS` and `posSelection` in every branch constructor would duplicate the input
  model's property logic and fail to preserve new properties.
* A full `branch -> PropertyMap` representation would support branch-specific output for arbitrary
  trees, but would require new raw formats, tree identities, accumulation rules, and viewers before
  the current workflow needs them.
* Restoring the removed `MixtureModel` category would recreate special binding and Haskell handling
  that `Discrete (BranchModel m)` no longer needs.

### 7. Two-ratio branch model

**Choice:** Construct the maintained two-category branch model with the shared foreground/background
constructor. Parameterize the effective foreground value as

```text
omega_foreground =
    omega_background                                      when H = 0
    omega_background * foreground_ratio                   when H = 1.
```

Use `omega_background ~ logLaplace(-1, 1)`, `foreground_ratio ~ logLaplace(0, 1)`, and
`H ~ bernoulli(0.5)` as documented defaults. Keep `foreground_ratio` in the state under both
hypotheses. Under `H = 0` it is unused by the likelihood, so its proper prior integrates to one and no
dimension-changing parameter is needed for this comparison.

Scale each single-category foreground and background substitution model to expected rate one before
constructing the `BranchModel`. This gives every branch the usual unit-rate normalization and makes
the branch-length interpretation independent of its category. Validate this choice against a small
PAML branch-model likelihood; if it does not agree, return to the normalization design instead of
changing expected output.

The alternative allows both relaxation and intensification. Its model probability therefore concerns
branch heterogeneity, matching the PAML branch comparison. Report model difference, effective
foreground `omega`, and the two direct probability statements listed above. The prior on
`foreground_ratio` materially affects the Bayes factor and must be visible in the generated model and
report.

**Alternatives:**

* A direct null `omega_foreground <= 1` versus alternative `omega_foreground > 1` answers a cleaner
  positive-selection question but is not the Bayesian counterpart of PAML's branch comparison.
* Independent foreground and background parameters are equivalent at the likelihood level but make a
  sensible prior on their difference less clear.
* Scaling only the combined foreground/background process could make branch lengths depend on the
  number and lengths of foreground branches. It would also differ from the per-branch-category
  normalization used by the current branch-site model.

### 8. Foreground identity

**Choice:** Generated branch and branch-site workflows require a fixed topology with foreground edge
attributes supplied before the analysis. Branch lengths may remain changeable. Enforce this once in
generated-model construction rather than adding checks to likelihood or transition-matrix hot paths.

**Alternatives:**

* Tracking a foreground clade through topology changes could support topology inference, but needs
  definitions for clade identity, proposals in which that clade is absent, and branches whose
  endpoints change.
* Letting a foreground marker travel with an edge object through topology proposals is simpler, but
  then the biological split being tested can change. That is a different analysis rather than a
  transparent generalization of a prespecified foreground test.

### 9. Tool ownership

**Choice:** Extend the existing tools:

* `statreport` owns stable model-probability aggregation;
* `calc-properties` owns per-character posterior moments;
* `alignment-draw` owns ranked and alignment-projected site presentation; and
* `bp-analyze` owns the combined run report.

Do not add a workflow executable. Add tool tests as declarative directories, using `check.py` only
where exact output comparison cannot express the required graphical or structural assertion.

### 10. Model choices

**Choice:** Recommend `m2a_test` and `m8a_test`, not a literal Bayesian M7-versus-M8 comparison. The
M8a point null retains a neutral class and avoids interpreting a poor M7 fit as positive selection.
Use ten beta categories in the documented PAML-comparison workflow; the ordinary binding should
retain its current faster default of four.

Recommend joint alignment by default. Fixed-alignment analyses remain useful for comparison, and
branch or branch-site analyses retain a fixed, labeled topology because foreground identity is part
of the predeclared biological question.

### 11. Many genes and repeated branch tests

**Choice for the first implementation:** Report per-gene Bayes factors and allow a user-specified
prior prevalence `pi` to convert them to posterior probabilities. Do not add a new interchange format
until a cross-gene consumer and its required inputs have been designed.

**Later alternative:** Estimate `pi` under a beta prior from a collection of per-gene Bayes factors.
This is less invasive than running all genes in one joint MCMC, but needs its own reviewed numerical
design. Independent prior probability one half for every gene should not be presented as an automatic
multiple-testing solution.

### 12. Scope of validation

The workflow should identify what was fitted, not attempt to certify that the biological assumptions
are true. It should retain the genetic code, codon model, codon-frequency model, alignment mode,
topology and foreground definition, priors, number of chains, retained samples, and convergence
diagnostics.

Sensitivity runs should be documented for multinucleotide changes, synonymous-rate variation, and
alternative codon-frequency models. Recombination and biased gene conversion are limitations to
report, not conditions that BAli-Phy can currently diagnose away.

## Review of the revised first-stage plan

Review of the first-stage update found the following problems and changed the final plan accordingly:

* It initially carried forward the old proposal to delete `BranchModel`, but current `branch_site`
  uses that type directly. The final plan instead makes it the common branch representation.
* It initially treated foreground property creation as unfinished. The current code already preserves
  all foreground and background input properties, so the remaining task is to share that construction
  with the branch comparison and report the results.
* It referred to obsolete Python tool-test modules. The final plan uses the current declarative
  per-case test format.
* It understated the current `alignment-draw` interface. The plan now adds only posterior SD, semantic
  handling for prefixed probability properties, and ranked-site navigation.
* A first branch-model draft did not specify normalization. The final plan selects per-category
  unit-rate normalization and requires comparison with PAML before accepting it.
* General property metadata would be cleaner than name recognition, but would add changes across
  several established interfaces for two current names. The final plan records the narrow suffix rule
  as non-ideal and leaves metadata for a design that has more consumers.
* A separate context-computation language and a new workflow command would duplicate working
  infrastructure. The final plan generalizes one binary-indicator helper and keeps reporting in the
  existing tools.
* The final plan keeps gene-level model evidence, site-level class probability, and branch
  heterogeneity as distinct quantities. It does not call all of them positive-selection
  probabilities.

The specified plan below contains only concrete implementation work. Items 1 through 12 are separate
`jj` commits. Items 13 and 14 verify the resulting series and do not create commits. A fix to an
earlier change should be made in an empty child of that change and squashed into it once verified.

## Specified implementation plan

1. **Generalize binary-indicator context fields.**
   Add a helper in `haskell/Probability/Random.hs` that accepts a short result label and a modifiable
   binary indicator, computes `condLogOdds indicator 1 2` once, and returns a `ContextAction Object`
   containing `LogOdds<label>` and `Pr<label>`. Define `positiveSelectionFields` using
   `label = "PosSelection"` without changing its generated field names. Extend
   `tests/haskell/MCMC/ContextConditionalLogger` to check the generic names and verify that the
   probability agrees with the log odds returned in the same object. Retain the existing
   `Numeric/ProbLogOdds` coverage for `-inf` and `+inf`. Do not add a C++ executable.

2. **Add stable conditional-log-odds summaries to `statreport`.**
   Recognize fields whose final name component begins with `LogOdds`. For every such field, accumulate
   `log(sigmoid(l))` and `log(sigmoid(-l))` with stable `logsumexp`, then emit:

   ```text
   <field>:  posterior-probability = <probability>     [log-odds = <value>]
   ```

   Suppress the default median in this mode unless explicitly requested. Accept `-inf` and `+inf`;
   reject NaN with the field name. Add declarative cases under `tests/tools/statreport/` proving that
   the result differs from the arithmetic mean of log odds and covering finite values, one-sided
   infinity, and mixed infinities.

3. **Enforce a stable foreground topology in generated models.**
   In `src/models/A-T-prog.cc`, detect use of generated `branch_categories` state and reject the model
   unless either the tree or its topology is fixed. Preserve changeable branch lengths and the current
   Newick `foreground` attribute mechanism. Add generated Haskell model cases for an accepted fixed
   topology and a concise error for an unfixed topology. Keep this check out of likelihood and matrix
   hot paths.

4. **Share foreground/background `BranchModel` construction.**
   Add one constructor to `haskell/SModel/BranchSite.hs` that combines branch categories, a background
   model, and a foreground model, and prefixes every property from the two input models with
   `background-` and `foreground-`. Refactor `branchSite` to call it without changing matrices,
   normalization, property names, or the public binding. Extend `tests/haskell/SModel/Properties` to
   retain its arbitrary-property check and run both testiphy branch-site likelihood cases.

5. **Add the two-ratio Bayesian branch comparison.**
   In the next commit, use the shared constructor to add a Haskell branch-comparison function and
   `bindings/models/branch_test.json`. Give the binding `omega_background ~ logLaplace(-1,1)`,
   `foreground_ratio ~ logLaplace(0,1)`, `branchDifference ~ bernoulli(0.5)`, `branch_cats`, and a
   codon-model function argument. Scale the single background and foreground models to expected rate
   one, use `omega_background` under the null, and use
   `omega_background * foreground_ratio` on foreground branches under the alternative.

   Use the binary-indicator helper with `label = "BranchDifference"` to generate
   `LogOddsBranchDifference` and `PrBranchDifference`. Add ordinary computed fields
   `foregroundOmega`, `foregroundOmegaAboveOne`, and `foregroundOmegaAboveBackground`. Add
   `NoImplicitPrelude` Haskell tests for both indicator states, prefixed input properties, and expected
   transition matrices. Add a small PAML/testiphy branch-model comparison and require identical
   likelihoods within the existing likelihood tolerance. If normalization disagrees, stop and revise
   the design instead of replacing the expected value.

6. **Document more accurate model-probability summaries.**
   Preserve the existing model explanations while updating the descriptions for `m2a_test`, `m3_test`,
   `m8a_test`, `branch_site`, `busted`, `busted_s`, and `branch_test`. Explain that the posterior mean
   of `PrPosSelection` or `PrBranchDifference` estimates the corresponding posterior probability with
   less Monte Carlo error than averaging the sampled indicator. For probabilities extremely close to
   zero or one, explain that `statreport` uses the matching `LogOdds*` field to report the posterior
   log odds accurately, and say not to average log odds directly. Retain the correct equal-prior
   Bayes-factor statements. Document the `background-*` and `foreground-*` branch-site properties and
   the distinct meaning of the branch-difference indicator.

7. **Add posterior SD to character-property summaries.**
   Replace each sum-only accumulator in `scripts/calc-properties` with a streaming count, mean, and
   centered second moment. Emit population posterior SD as an optional `sd` array next to `mean` and
   `count`, clamping only a negative roundoff remainder to zero. Extend
   `tests/scripts/test_calc_properties.py` with constant, varying, missing-value, and pooled-chain
   examples. Retain format version 1 because the field is optional and existing readers ignore
   unknown fields.

8. **Display posterior variation and recognize prefixed probability properties.**
   Teach `src/tools/alignment-draw.cc` to accept and validate an optional `sd` matrix and pass it to
   `src/tools/alignment-draw.js`. Include SD in property tooltips. Treat the exact name
   `posSelection` and names ending in `-posSelection` as probability properties for scale and
   threshold choices. Add the brief NOTE described in the design investigation. Add declarative
   alignment-draw cases for old summaries without SD, summaries with SD, malformed SD shape, and
   `foreground-posSelection`; extend the existing JavaScript scale test rather than adding a runner.

9. **Add a ranked-site view to `alignment-draw`.**
   Add a sequence selector and ranked table for the selected property. Show the one-based ungapped
   model-character position, displayed character, posterior mean, SD when present, and retained count.
   For codon alphabets, use the configured `Codons` translation table to display the corresponding
   amino acid. Provide probability thresholds including 0.5, 0.95, and 0.99, and make selecting a row
   locate the matching alignment cell by ungapped character identity. Add declarative cases with
   `check.py` for structural DOM or payload checks and focused JavaScript tests for ranking, threshold,
   and cell lookup.

10. **Add selection evidence to `bp-analyze`.**
    Detect `LogOddsPosSelection` and `LogOddsBranchDifference` fields. Invoke `statreport` once and
    parse the `posterior-probability` records it automatically emits for those fields instead of
    reading traces again. Report posterior probability,
    posterior log odds, the explicit prior probability, and `log BF10`. Add
    `--selection-prior`, defaulting to 0.5 to match maintained bindings, and print the chosen value.
    Also summarize the ordinary branch fields and link each partition to available ranked
    `posSelection`, `foreground-posSelection`, `dNdS`, and `foreground-dNdS` views. Extend
    `tests/scripts/test_bp_analyze_properties.py` for one partition, multiple partitions, missing
    property streams, branch evidence, and non-equal prior odds.

11. **Document the complete beginner workflow.**
    Update `doc/README.itex.xml` and `doc/Tutorial.tut.xml` with concrete M0, `m2a_test`,
    `m8a_test(n=10)`, `branch_test`, and `branch_site` examples. Include
    `--set write-properties=true`, multiple chains, `bp-analyze`, a selected reference sequence, and a
    fixed foreground-labeled topology where required. Explain the M8a rather than M7 null, posterior
    probability versus Bayes factor, unconditional site probabilities, one-based display versus
    zero-based stored coordinates, and branch heterogeneity versus `omega_foreground > 1`.

12. **Document sensitivity analysis and repeated tests.**
    Add a compact sensitivity checklist covering codon frame and genetic code, orthology, foreground
    choice before analysis, convergence, prior sensitivity, multinucleotide mutations,
    synonymous-rate variation, codon frequencies, fixed versus inferred alignment, recombination, and
    biased gene conversion. Explain how a user-supplied prevalence changes posterior odds through
    `BF * pi/(1-pi)`. Defer estimating a shared prevalence until its inputs and numerical method have
    a separate design.

13. **Verify the complete workflow.**
    Build with `nice -n10 ninja -C ../build/gcc-16-debug-O -j11`. Run the focused context-field,
    `statreport`, property, alignment-draw, `bp-analyze`, generated binding, branch-model, and
    testiphy likelihood cases, then the broader Haskell suite and the required 5d `+A` test. Run a
    small fixed-alignment codon example under M0, site, branch, and branch-site models and confirm that
    probability, odds, and Bayes-factor identities agree numerically with the traces. A debug-machine
    build is unnecessary unless an interpreter-specific failure appears.

14. **Check optional-work cost.**
    Use installed, warmed GCC 16 release builds to compare a short codon run before and after the
    series with property writing disabled, then enabled. Use at least seven interleaved `perf stat`
    runs when elapsed time is noisy. The disabled case should not gain meaningful MCMC work; the
    enabled case may pay for logging, while posterior moments and ranking remain post-processing
    costs. Prefer the simpler implementation when instruction and cycle differences remain within
    noise.

## Completion criteria

The first complete workflow should let a user:

* estimate gene-wide `dN/dS`;
* test site-level, branch-level, and branch-site hypotheses without confusing their meanings;
* obtain a Rao-Blackwellized posterior probability and correctly calculated posterior odds or Bayes
  factor;
* identify and inspect reference-sequence characters with high posterior positive-selection
  probability;
* see posterior mean and SD of site `dN/dS`;
* retain alignment uncertainty rather than assigning scientific identity to a sampled alignment
  column;
* reproduce the foreground definition and major model and prior choices; and
* recognize when a result needs prior, model, alignment, or multiple-testing sensitivity analysis.

## Current character-property implementation plan

This plan supersedes items 7--9 of the earlier implementation plan. Conditional property summaries
and producer-supplied property metadata remain separate future designs.

The implementation uses one C++ `character-properties` command with two subcommands:

```text
character-properties summarize [OPTIONS] SAMPLE-FILE [...]

character-properties report SUMMARY ALIGNMENT
    --alphabet ALPHABET
    --property NAME
    --kind property|positive-selection
    [--format text|tsv|json]
    [--sort column|mean-ascending|mean-descending|sd-descending]
    [--minimum-probability P]
```

Summary calculation, alignment projection, and report interpretation remain separate internal
stages. The intermediate summary is retained because its exact median is expensive to compute and
the same summary may be projected onto several template alignments.

1. **Move unconditional summarization to C++.**
   Add shared summary data and aggregation code under `src/alignment/`, and add the
   `character-properties summarize` subcommand. Preserve sample selection, Welford population SD,
   the exact memory-bounded lower median, and the 256 MiB default memory target. Require complete
   observed leaf-character states and stable sample shapes. Emit finite `mean`, `sd`, and `median`
   matrices with only top-level retained-sample counts. Compare structured output and failures with
   the Python implementation, then remove `scripts/calc-properties`, its installation rule, and its
   Python test rather than retaining a compatibility wrapper. Convert its coverage to declarative
   tool cases and update `bp-analyze`.

2. **Centralize summary consumption.**
   Move summary validation and alphabet-width alignment tokenization from `alignment-draw.cc` into
   the shared alignment component. Require every displayed sequence to have exactly one summary
   value per non-gap character while allowing unused extra summary sequences. Refactor
   `alignment-draw` to use the shared representation without changing its output.

3. **Add alignment-independent column projection.**
   Add a shared projection component that records template columns and their sequence-character
   members. It must not choose representatives, pair properties, rank columns, or apply report
   thresholds. Attach codon translations using the actual configured `Codons` alphabet.

4. **Implement generic property reports.**
   Add an explicit report kind and compact report specification. For descending means, ascending
   means, and descending SDs, select the character with the most extreme requested statistic in each
   template column; use maximum mean for column-ordered reports. Break ties by displayed sequence
   order, omit all-gap columns, and emit text, TSV, and versioned JSON containing the same records.
   Add declarative cases for rates, uncertainty, ties, gaps, coordinates, codons, and all formats.

5. **Implement positive-selection reports.**
   Recognize `posSelection` and names ending in `-posSelection`, with a NOTE that this name-based
   policy is non-ideal. Pair them with the corresponding `dNdS` property when it exists. Rank by the
   largest character probability in each column, default to a 0.5 threshold, and report probability
   plus paired dN/dS mean, SD, and median. Cover 0.5, 0.95, and 0.99 thresholds, missing companions,
   prefixed names, and rejected non-probability properties.

6. **Make original alignment rendering alphabet-aware.**
   Allow `alignment-draw --alphabet` without `--properties`, and make `bp-analyze` always pass the
   canonical partition alphabet. Keep one table cell per logical character, render codon nucleotide
   spans with their original colors, use the configured genetic code, and put complete one-based
   ruler labels in logical character columns.

7. **Organize the viewer JavaScript.**
   Split numerical scales and palettes, property/report decoding, and DOM interaction into three
   source files embedded with the existing build helper. Preserve behavior while making this
   structural change, and use one existing Node test entry rather than adding a runner.

8. **Display complete unconditional summaries.**
   Show property mean plus or minus SD and median in tooltips, and show the shared retained-sample
   count once. Replace the `Original colors` menu item with an independent checkbox, keep property
   inspection active in original-color mode, and preserve legend height so the alignment does not
   shift vertically.

9. **Add generic and specialized ranked panels.**
   Build report rows in shared C++ and embed them in the viewer; JavaScript must not independently
   select representatives or pair properties. Add a fixed-height ranked panel with generic sort
   controls and positive-selection probability thresholds. Selecting a row highlights its template
   column, focuses its representative, and opens the tooltip. Require browser rows to agree with
   `character-properties report --format=json`.

10. **Document and verify the result.**
    Replace the `calc-properties` manual with a `character-properties` manual and update
    `alignment-draw` and `bp-analyze` documentation. Explain character versus template-column
    coordinates and generic versus specialized reports. Build with
    `nice -n10 ninja -C ../build/gcc-16-debug-O -j11`; run all focused declarative and Node tests,
    a small codon positive-selection workflow, and the required 5d `+A` test. Compare the C++ and
    temporary Python summarizers for output, failures, wall time, and peak memory before deleting the
    Python version. Debug-machine and MCMC hot-path benchmarks are unnecessary unless an
    interpreter-specific problem appears.
