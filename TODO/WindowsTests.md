# Complete native Windows testing

## Goal

Make `windows-native-mingw` exercise the supported native Windows build,
Meson test suite, installed programs, `bp-analyze`, and testiphy.  The stale
runtime-serialization test selector has already been repaired and is omitted
from this plan.

## Review conclusions

1. The full Meson suite should replace the hand-maintained list of native
   smoke tests.  Keeping both would duplicate coverage and allow the list to
   become stale again.
2. The repository's tracked test-data symlinks must be restored before the
   full suite runs; otherwise failures report missing or malformed inputs
   rather than Windows product defects.
3. Cairo must be present when Meson configures the build so that `draw-tree`
   and the complete installed tool set are built.
4. Build-tree tests and installed-prefix tests cover different contracts and
   should remain separate.  The former validate Meson targets; the latter
   validate installation layout, runtime DLL discovery, and scripts.
5. No new workflow, helper script, executable test, or `workflow_dispatch`
   path is justified.  The native commands are short, already have a clear
   owner in `build.yml`, and a helper would not reproduce GitHub's host.
6. GitHub runs should be evidence checkpoints rather than permanent commit
   boundaries.  Product and workflow changes still belong in separate `jj`
   commits.

## CI iteration

For each checkpoint, place a disposable child change above the permanent
series and temporarily reduce the build matrix to `windows-native-mingw`.
Move a dedicated CI bookmark to that child and push it without opening a pull
request, since the current workflow would otherwise run for both `push` and
`pull_request`.  Diagnostic logging and the reduced matrix remain only in
this disposable change.

Before pushing, run `actionlint` when available, list the configured Meson
tests to catch stale selectors, run applicable Windows tests through the
cross build and Wine, and run `bali-phy:computation` plus
`bali-phy:bali-phy 5d +A 50` in `build/gcc-16-debug-O`.

After a checkpoint succeeds, retain the permanent fixes, rebase the
disposable child onto the new head, and amend it for the next checkpoint.
Abandon the child and its bookmark before the final full-matrix run.

## Specified commits

Each numbered item is a separate `jj` commit.  Failures exposed by the first
checkpoint get one additional commit per independent cause.

1. **Build the complete native tool set.**  Add
   `mingw-w64-x86_64-cairo` to the packages installed by the
   `windows-native-mingw` matrix entry in `.github/workflows/build.yml`.
   Confirm in the CI configure summary that Cairo is `system`, and confirm
   that the build and install steps produce `draw-tree.exe` under `build/`
   and `local/bin/`.

2. **Restore tracked test-data symlinks.**  Add a native-only `msys2 {0}`
   step after MSYS2 setup and before configuration.  Set the checkout's local
   `core.symlinks` value to `true`, force the index contents back into the
   work tree with `git checkout-index --force --all`, and check every tracked
   symlink with:

   ```sh
   git ls-files -s tests | awk '$1 == "120000" {print $4}' |
     while IFS= read -r link; do test -L "$link"; done
   ```

   Precede the commands with a brief compatibility note explaining that the
   re-checkout compensates for Windows checkout materializing tracked
   symlinks as ordinary files, and that it can be removed when checkout
   preserves them directly.  Keep this logic inline rather than adding a
   repository script.

3. **Run the complete Meson suite.**  Replace the explicit native-Windows
   test-name list with:

   ```sh
   meson test -C build --print-errorlogs --num-processes 2
   ```

   Keep `local/bin` first in `PATH` so tests and subprocesses can find the
   installed DLLs and tools.  At the first CI checkpoint, record every failing
   Meson test and its log.  For each independent failure, reproduce the named
   test through the cross build when possible, then make one focused commit:
   fix product code for a Windows defect, make the test portable for an
   invalid POSIX assumption, or add a missing declared MSYS2 dependency.
   Do not add a Windows `xfail` for behavior that is intended to be supported.
   Rerun each named failure and then the complete native suite.

   Push the cumulative head through the disposable native-only child after
   this commit.  Do not proceed to installed-prefix coverage until the full
   build-tree suite passes.

4. **Test the installed command-line programs.**  Add a native-only step
   after the Meson suite that creates a clean directory under
   `$RUNNER_TEMP`, puts `$PWD/local/bin` first in `PATH`, and invokes the
   installed `bali-phy.exe` directly for `help`.  Run that first command with
   a restricted `PATH` containing only `local/bin` and the Windows system
   directories, so missing installed runtime DLLs cannot be supplied by
   `/mingw64/bin`.  Restore the MSYS2 `PATH`, copy the installed
   `5d-muscle.fasta` example into the clean directory, run `bali-phy --test`,
   run a 50-iteration chain, and run the installed `statreport` on the
   resulting log.  Use fixed seeds and paths under the clean directory; do
   not use build-tree executables or source-tree example data.

5. **Exercise an installed `bp-analyze`.**  Extend the installed-program
   step to run two 50-iteration chains with distinct fixed seeds, then run
   `bp-analyze --outdir=Results` on both output directories.  Assert that
   `Results/index.html`, `Results/Report`, and
   `Results/greedy-tree.svg` exist so a zero exit status cannot hide a failed
   subprocess.  Add a native package only when this checkpoint identifies a
   required program that is absent; optional R or gnuplot plots are not a
   reason to weaken the core report and `draw-tree` checks.

   Push the cumulative head through the disposable native-only child after
   the installed CLI and `bp-analyze` checks both pass locally where possible.

6. **Run testiphy natively.**  Remove the native-Windows exclusion from the
   existing retrying `Download testiphy test suite` step and run the checkout
   from the MSYS2 shell against the installed `bali-phy.exe`.  Preserve the
   existing removal of
   `tests/likelihood/one-sequence`, then run both the variable-alignment form
   and the `-Inone` fixed-alignment form with fixed seeds.  Keep the clone and
   invocations in `build.yml`; do not add a testiphy wrapper to this
   repository.

   Push this as the third native-only checkpoint before restoring the complete
   matrix.

7. **Finish and clean up.**  Remove the disposable matrix restriction and
   diagnostic changes, run `actionlint`, and push the clean cumulative head
   for a complete workflow run across every matrix entry.  Require the full
   native Meson suite, installed-program checks, `bp-analyze`, and both
   testiphy modes to pass.  Then remove the completed native-Windows item from
   `TODO/TODO.md` and remove this plan when it no longer records unfinished
   work.

## Completion criteria

- `windows-native-mingw` configures with Cairo and installs the complete tool
  set.
- Every configured Meson test passes on the native Windows runner.
- The installed `bali-phy`, `statreport`, and `bp-analyze` work from a clean
  directory using the installed prefix.
- Variable- and fixed-alignment testiphy runs pass natively.
- No temporary CI-only matrix restriction, diagnostic code, or additional
  test executable remains in the final series.
