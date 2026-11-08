#!/usr/bin/env python3

# Prefer ordinary declarative tests using command.txt and expected output/error/exit files.
# Add tests here only when they cannot be expressed in the existing non-script test formats,
# for example because they must inspect generated source or reuse it across commands.

import argparse
import json
import os
import pathlib
import shlex
import subprocess
import sys
import tempfile
import time


# Identify and time each phase, checking its exit status while retaining output for diagnostics.
def run_command(phase, command, work_directory, expected_exit=0):
    print(f"Starting: {phase}", flush=True)
    start = time.monotonic()
    result = subprocess.run(command, cwd=work_directory, encoding="utf-8", capture_output=True)
    print(f"Finished: {phase} ({time.monotonic() - start:.2f}s)", flush=True)
    if result.returncode != expected_exit:
        raise AssertionError(f"{phase}: expected exit {expected_exit}, got {result.returncode}\n"
                             f"Command: {shlex.join(command)}\n{result.stdout}{result.stderr}")
    return result


# Reuse generated source in both runtime modes; single-command tests cannot check this handoff.
# This becomes obsolete if infer no longer retains runnable source.
def standalone_test(command, work_directory):
    generate = command + [
        "input.fasta",
        "--imodel=none",
        "--smodel=TN93",
        "--iterations=0",
        "--name=generated",
    ]
    run_command("generate standalone source", generate, work_directory)

    source = work_directory / "generated-1" / "BAliPhy.Main.hs"
    if not source.is_file():
        raise AssertionError("the initial run did not retain BAliPhy.Main.hs")

    run_generated = command + [
        "run",
        str(source.relative_to(work_directory)),
    ]

    directories_before_test = {path for path in work_directory.rglob("*") if path.is_dir()}
    logger_files_before_test = {
        path for path in work_directory.rglob("C1.*") if path.is_file()
    }
    run_command("execute test mode", run_generated + ["--test"], work_directory)
    directories_after_test = {path for path in work_directory.rglob("*") if path.is_dir()}
    logger_files_after_test = {
        path for path in work_directory.rglob("C1.*") if path.is_file()
    }
    if directories_after_test != directories_before_test:
        raise AssertionError("the retained program created a directory in test mode")
    if logger_files_after_test != logger_files_before_test:
        raise AssertionError("the retained program created a logger file in test mode")

    output_directory = pathlib.Path("standalone") / "nested"
    (work_directory / output_directory).mkdir(parents=True)
    standalone = run_generated + [
        "--output-dir",
        str(output_directory),
        "--log-format=json",
    ]
    run_command("execute normal mode", standalone, work_directory)

    output_paths = [
        work_directory / output_directory / name
        for name in ["C1.log.json", "C1.trees"]
    ]
    if not all(path.is_file() for path in output_paths):
        raise AssertionError(f"the standalone program did not create its log files: {output_paths}")

    # Preserve logging of structured model parameters across representation changes;
    # successful generated-program execution alone does not establish that the field survived.
    log_records = output_paths[0].read_text(encoding="utf-8").splitlines()
    sample = json.loads(log_records[1])
    frequencies = sample["parameters//"]["S1/"]["TN93:pi"]
    if not isinstance(frequencies, dict) or set(frequencies) != {"A", "C", "G", "T"}:
        raise AssertionError(f"TN93 frequencies were not logged as a JSON object: {frequencies}")

    if (work_directory / output_directory / "C1.log").exists():
        raise AssertionError("the standalone program ignored its runtime log format")
    if (work_directory / output_directory / "C1.run.json").exists():
        raise AssertionError("the standalone program unexpectedly created the C++-owned run manifest")


# Bypass early C++ validation to check the guard in retained Haskell before it creates output.
# This becomes obsolete if fixed alignments become valid during MCMC.
def fixed_alignment_test(command, work_directory):
    fixed_generate = command + [
        "input.fasta",
        "--fix=alignment",
        "--test",
    ]
    run_command("generate fixed-alignment source", fixed_generate, work_directory)

    fixed_source = work_directory / "BAliPhy.Main.hs"
    fixed_run = command + [
        "run",
        str(fixed_source.relative_to(work_directory)),
        "--name=fixed-retained",
    ]
    fixed_result = run_command("reject fixed-alignment execution", fixed_run, work_directory, expected_exit=1)
    fixed_error = "Currently --fix=alignment only works with --test."
    if fixed_error not in fixed_result.stderr:
        raise AssertionError("Missing fixed-alignment diagnostic:\n" + fixed_result.stdout + fixed_result.stderr)
    if (work_directory / "fixed-retained-1").exists():
        raise AssertionError("the retained fixed-alignment program created an output directory")


# Inspect generated source and JSON reports: likelihood tests alone miss duplicated constructors,
# missing named frequencies, and fresh-name collisions. Keep this while computed reports share results.
def result_reports_test(command, work_directory):
    run_command("execute shared model reports", command + [
        "input.fasta", "--imodel=none", "--smodel=nonRev +> nonEq",
        "--tree=~UniformRootedTree(taxa)", "--iterations=0", "--log-format=json",
        "--name=reports",
    ], work_directory)
    directory = work_directory / "reports-1"
    source = (directory / "BAliPhy.Main.hs").read_text()
    if source.count("nonRev'") != 1:
        raise AssertionError("computed reports duplicated the nonRev constructor")
    records = (directory / "C1.log.json").read_text().splitlines()
    model_log = json.loads(records[1])["parameters//"]["S1/"]
    for name in ("nonRev:eqFrequencies", "nonEq:initialFreq"):
        if set(model_log[name]) != {"A", "C", "G", "T"}:
            raise AssertionError("missing named frequencies: " + name)

    expression = ("result + result_2 + loggers where "
                  "{result=~Exponential(1); result_2=2; loggers=~Exponential(1)}")
    for attempt in range(2):
        run_command("allocate readable final names", command + ["print", expression], work_directory)
        source = (work_directory / "Print.Main.hs").read_text()
        if "result_3" not in source or "loggers_2" not in source:
            raise AssertionError("final packaging reused occupied local names")
        if attempt == 0:
            first_source = source
        elif source != first_source:
            raise AssertionError("independent generation changed local name numbering")

    # Catalogue reports do not contain colliding lambda binders. Exercise substitution under
    # nested lambdas and function-call heads, then force the reports in the retained program.
    bindings = work_directory / "capture-package" / "bindings"
    bindings.mkdir(parents=True)
    binding = {"name": "captureTest", "result_type": "Bool", "call": "not(@x)",
               "args": [{"name": "x", "type": "Bool"}], "computed": [
        {"name": "direct", "value": "map(|captureTest_model: @result|,[False])"},
        {"name": "nested", "value":
         "map(|captureTest_model: map(|captureTest_model: @result|,[False])|,[False])"},
        {"name": "adjacent", "value":
         "map(|f: map(f,[False])|,map(|z: (|z: @result|)|,[False]))"},
        {"name": "functions", "value":
         "map(|x: (x(@x),map(|x: @x|,[True]),(x(@x),map(|x_2: x(@x)|,[True])))|,[not])"},
    ]}
    (bindings / "capture.json").write_text(json.dumps(binding))
    separator = ";" if os.name == "nt" or any(";" in arg for arg in command) else ":"
    capture_command = [arg + separator + "capture-package" if arg.startswith("--package-path=")
                       else arg for arg in command]
    run_command("generate capture-free reports", capture_command + [
        "print", "captureTest(x) where {x=1<0}"], work_directory)
    source = (work_directory / "Print.Main.hs").read_text()
    source = "import qualified Data.JSON as J\nimport qualified Data.Text as Text\n" + source
    source = source.replace("show $ fst result",
                            "Text.unpack $ J.encode $ J.Object $ parameterLogValues $ snd result")
    (work_directory / "Capture.hs").write_text(source)
    result = run_command("evaluate capture-free reports", command + ["run", "Capture.hs"], work_directory)
    if json.loads(result.stdout) != {
        "captureTest:direct": [True], "captureTest:nested": [[True]], "captureTest:adjacent": [[True]],
        "captureTest:functions": [[True, [False], [True, [True]]]],
    }:
        raise AssertionError("template lambdas captured substituted variables: " + result.stdout)


# Each Meson case owns its work directory; only commands that reuse generated source run together.
def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--case", required=True, choices=["standalone", "fixed-alignment", "result-reports"])
    parser.add_argument("--wrapper", action="append", default=[])
    parser.add_argument("executable")
    parser.add_argument("package_path")
    args = parser.parse_args()

    with tempfile.TemporaryDirectory(prefix=f"bali-phy-generated-{args.case}-") as tmp:
        work_directory = pathlib.Path(tmp)
        (work_directory / "input.fasta").write_text(">one\nACGT\n>two\nACGT\n", encoding="utf-8")
        command = args.wrapper + [args.executable, "--seed=1", args.package_path]
        if args.case == "standalone":
            standalone_test(command, work_directory)
        elif args.case == "fixed-alignment":
            fixed_alignment_test(command, work_directory)
        else:
            result_reports_test(command, work_directory)
    return 0


if __name__ == "__main__":
    sys.exit(main())
