# Automated performance regression testing

## Recommendation

Gate pull requests on a same-job comparison of the tested merge candidate
against its base revision.  Use retired user-space instructions as the primary
metric.  Record wall time and other hardware counters as diagnostics, but do
not initially use them as blocking metrics.  Pair measurements only when the
two revisions produce identical stochastic traces.

The basic comparison should be:

```text
checkout base and merge candidate
build both with the same compiler, flags, ISA, and dependencies
warm up both binaries and compare their sentinel-seed MCMC traces
select the controlled or distributional statistic described below
upload raw results and fail if the configured budget is exceeded
```

This is preferable to comparing a PR with historical timings from unrelated
GitHub-hosted runners.  The base and candidate measurements share one VM, CPU,
kernel, and toolchain installation.  Historical measurements remain useful for
trends, but must not determine the primary PR gate.

## Why use retired instructions

Retired instructions are substantially less sensitive than elapsed time to
dynamic frequency scaling, turbo state, and competing host activity.  For a
deterministic workload, repeated instruction counts are often stable enough to
detect small changes.

An instruction-count gate does not detect every performance regression:

* equal instruction counts can have different cache-miss or branch-miss rates;
* a vectorized implementation can do more work per instruction;
* synchronization, system calls, I/O, and blocking can dominate elapsed time;
* memory layout or allocation changes can increase stalls without adding many
  instructions; and
* hardware-counter semantics are not guaranteed to be identical across CPU
  families or virtual-machine implementations.

The gate should therefore record at least wall time and, when reliably
available, cycles, branches, branch misses, cache misses, task clock, and CPU
migrations.  These are diagnostic signals rather than initial blocking
metrics.  Avoid requesting more simultaneous hardware events than the PMU can
count without multiplexing.

Use the user-space event explicitly:

```bash
perf stat --json-output \
  -e instructions:u \
  -o result.json \
  -- command arguments...
```

Counting `instructions:u` excludes kernel instructions, which improves
stability but also means that changes in kernel work are not covered by the
primary metric.

The collector must reject a measurement when:

* the event is unsupported or not counted;
* `perf` lacks permission to access the PMU;
* the benchmark command fails;
* the expected result file is absent;
* the counter's running percentage is below an accepted minimum, such as
  99.5%; or
* an expected benchmark case was not executed.

Do not silently replace a missing instruction count with wall time.  That
would make the meaning and reliability of the gate depend on which runner was
assigned.

A required job with insufficient valid measurements must return a non-passing,
inconclusive result rather than pass or drop the case.

## Same-job base-versus-candidate comparison

For a `pull_request` workflow, compare
`github.event.pull_request.base.sha` with GitHub's tested synthetic merge commit
(`github.sha`), not the unmerged PR head.  Create separate source, build,
install, and run directories; do not reuse a source tree or Meson build
directory across revisions.

Both builds must use the same:

* fixed runner image, such as `ubuntu-24.04`, rather than `ubuntu-latest`;
* compiler and compiler version;
* Meson and Ninja versions;
* release configuration and assertion settings;
* architecture option, for example `-march=x86-64-v3` if the selected runners
  are known to support it;
* dependency environment and variables, except dependency changes being
  measured as part of the candidate; and
* benchmark input files and command-line options.

The existing x86-64-v3 release entry in
[`build.yml`](../.github/workflows/build.yml) is a useful model for the build
configuration.  The performance job should nevertheless be a separate,
non-containerized job.  Job containers can introduce additional
`perf_event_open` restrictions, and benchmark results should not be mixed with
the ordinary build-and-test matrix.

Run the base and candidate binaries in a balanced order, for example:

```text
warm base
warm candidate
base, candidate, candidate, base, base, candidate,
candidate, base, base, candidate, candidate, base
```

Alternating the order prevents gradual thermal, frequency, or background-load
drift from consistently favoring one revision.  Run only one benchmark process
at a time.  Use an isolated temporary working directory for every invocation
so output such as `25-muscle-1/` cannot leak between measurements.  Input
generation, output cleanup, and result validation belong outside the measured
command whenever possible.

## BAli-Phy workloads and stochastic traces

The notes in [`benchmarks/BENCHMARKS.md`](../benchmarks/BENCHMARKS.md)
already identify interleaving as necessary.  The existing
[`scripts/benchmark.py`](../scripts/benchmark.py) also supplies a useful
end-to-end workload, but its in-place checkout and shared build/install
directories are not suitable for a CI comparison.

A possible initial end-to-end case is:

```bash
bali-phy examples/5S-rRNA/25-muscle.fasta \
  --seed=0 --iter=300 -Inone
```

BAli-Phy has an important complication: a source change can alter the
stochastic trace even with the same seed.  First compare the base and candidate
MCMC trace outputs for one fixed sentinel seed.  Identical traces select a
controlled comparison; differing traces select a distributional comparison,
with no assumed correspondence between equal seeds across revisions.

Combine deterministic same-work microbenchmarks with end-to-end cases using
this adaptive policy.  Scheduled full-grid runs can check whether the sentinel
remains representative.  Intentional changes in work may require a reviewed
budget update or override.

Every benchmark must validate output outside measurement to detect incorrect or
incomplete execution.

## Meson integration

Declare local benchmark workloads with Meson's `benchmark()` function.  Meson
runs benchmark entries separately from ordinary tests, does not run them in
parallel, and does not inject `MALLOC_PERTURB_`.  They can be invoked with:

```bash
meson test -C build --benchmark
```

See the Meson
[`benchmark()` documentation](https://mesonbuild.com/Reference-manual_functions_benchmark.html).

Meson should own the benchmark executable, arguments, dependencies, and local
developer entry point.  A separate Python comparison driver should own:

* base/candidate invocation order and trace classification;
* temporary working directories;
* separate `perf stat` invocations and line-delimited JSON parsing;
* validation of required counters and benchmark outputs;
* ratio calculation and threshold policy;
* human-readable and machine-readable reports; and
* the final pass/fail exit status.

The first implementation can describe a small fixed list of cases directly in
the comparison driver or a tracked manifest.  Meson introspection can be used
later if maintaining the same case list in two places becomes a problem.

## Regression policy

Choose the statistic from the trace comparison.  For a controlled comparison,
calculate each adjacent or balanced pair as:

```text
ratio = candidate instructions / base instructions
```

Gate on the median paired ratio.  For a distributional comparison, calculate:

```text
ratio = median(candidate instruction counts) / median(base instruction counts)
```

Report at least:

* every raw base and candidate count and seed;
* trace-match status and the selected comparison mode;
* paired ratios when applicable and the final ratio;
* a dispersion measure such as median absolute deviation;
* the number of valid and rejected samples; and
* the corresponding wall times and diagnostic counters.

Do not apply Welch's t-test to pooled historical repetitions.  Runs are
clustered by machine, so treating old repetitions as independent can greatly
overstate confidence.

Do not choose a permanent threshold before measuring the stability of the
actual cases on GitHub Actions.  A reasonable initial policy is:

* collect at least five valid repetitions per revision in controlled mode;
* use seven fixed seeds per revision in distributional mode;
* run in non-blocking shadow mode for at least two weeks;
* begin with a 3--5% per-case instruction budget;
* apply the budget to the ratio selected by the trace mode;
* treat insufficient or unstable data as inconclusive, not passing; and
* tighten individual budgets only after observing their normal variation.

Initially gate each important case independently.  A suite-wide geometric mean
can be reported, but it should not hide a large regression in one operation
behind small improvements elsewhere.

Intentional tradeoffs need an explicit escape hatch.  One possibility is a
maintainer-only `performance-override` label that leaves the regression visible
but allows an acknowledged slowdown to merge.  Trigger the workflow for
`labeled` and `unlabeled` activity or provide a trusted manual rerun; never use
`pull_request_target` to execute PR code.  The report must retain the exceeded
budget and raw data.

## GitHub Actions feasibility

Standard Linux GitHub-hosted runners are fresh virtual machines and provide
passwordless `sudo`, but GitHub does not guarantee that the virtual PMU is
available.  The first workflow should therefore be a non-gating capability
probe on the exact runner image intended for performance testing.

For example:

```bash
sudo apt-get update
sudo apt-get install -y linux-tools-common linux-tools-generic

cat /proc/sys/kernel/perf_event_paranoid
sudo sysctl -w kernel.perf_event_paranoid=2

perf stat --json-output \
  -e instructions:u \
  -o perf-probe.json \
  -- /bin/true
```

Linux controls unprivileged PMU access through `perf_event_paranoid` and the
`CAP_PERFMON` capability.  See the kernel
[`perf_events` security documentation](https://docs.kernel.org/admin-guide/perf-security.html).
GitHub's current hosted-runner model is documented in the
[`GitHub-hosted runners reference`](https://docs.github.com/en/actions/reference/runners/github-hosted-runners).

If the probe reports that hardware instructions are unsupported even after the
permission check, a standard hosted runner cannot provide this gate reliably.
Possible fallbacks are:

1. Use a dedicated self-hosted bare-metal runner for performance jobs.
2. Use Callgrind for small deterministic microbenchmarks, with separate
   baselines and budgets because its simulated `Ir` event is not hardware
   retired instructions.
3. Keep same-job wall-time measurements as non-blocking diagnostics until a
   suitable PMU-enabled runner is available.

Do not expose a valuable persistent self-hosted machine to arbitrary code from
public fork pull requests.  A self-hosted performance runner must either be
restricted to trusted commits/manual approval or be an isolated, disposable
machine with no useful credentials, private network access, or persistent
data.  GitHub discusses this risk in its
[`self-hosted runner guidance`](https://docs.github.com/en/actions/how-tos/manage-runners/self-hosted-runners/add-runners).

A single dedicated performance runner should execute only one performance job
at a time.  Add workflow concurrency control as a second safeguard against
overlapping measurements.

## Results and historical trends

Every workflow should upload a structured result artifact containing:

* base and candidate commit IDs;
* benchmark name, input, seed, and exact command;
* compiler, linker, Meson, Ninja, `perf`, kernel, and dependency versions;
* CPU model, microcode, architecture flags, and runner image;
* raw counter JSON and wall times for every invocation;
* rejected measurements and reasons;
* calculated ratios, thresholds, and final decision; and
* the workflow and job IDs.

Also write a concise GitHub job summary so the regression is understandable
without downloading artifacts.

A scheduled default-branch workflow may store results for trend plots.  Keep
this history partitioned by toolchain, ISA, CPU model, runner image, kernel, and
dependency set.  Do not make a PR pass or fail solely because its result differs
from measurements collected on older or different machines.

## Rollout

### Phase 1: capability probe

Add a manually triggered or non-blocking hosted-runner job that records the
runner metadata and verifies that `instructions:u` can be counted without
multiplexing.

### Phase 2: local comparison driver

Implement one deterministic case and the 25-muscle end-to-end case.  Verify
balanced interleaving, temporary-directory isolation, JSON parsing, output
validation, and clear error handling locally.

### Phase 3: non-blocking PR measurements

Build the PR base and merge candidate in the same job and upload comparisons
without failing the workflow.  Characterize controlled and distributional
variation, trace changes, and hosted-runner failures.

### Phase 4: enforce conservative budgets

Select only stable cases for gating, start with conservative per-case budgets,
and make the performance job a required check.  Retain a reviewed override for
intentional slowdowns.

### Phase 5: historical reporting

Add scheduled trend collection after the same-job PR gate is trustworthy.
Use it to diagnose gradual changes, not replace base/candidate comparisons.

## Lessons to reuse from Eigen

Reuse these parts of Eigen's benchmark design:

* keep benchmarks separate from correctness tests;
* compile an explicit release/ISA configuration;
* run benchmark processes serially;
* retain raw structured results and complete metadata;
* distinguish a small frequent suite from broader scheduled coverage;
* make missing expected coverage an error; and
* present regressions through the CI test/report interface.

Do not copy these aspects directly:

* gating a PR against pooled measurements from unrelated machines;
* treating repetitions from many workflow runs as independent samples;
* silently skipping failed benchmark executables or unsupported targets; or
* using wall time as the only blocking signal on dynamically scaled shared
  infrastructure.

The adaptive same-job instruction-count design is smaller than Eigen's
historical pipeline and should provide a more reliable first performance gate
for BAli-Phy.
