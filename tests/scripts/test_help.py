#!/usr/bin/env python3

import argparse
import json
import os
from pathlib import Path
import re
import tempfile
import subprocess


# Run a BAli-Phy command while retaining its status and diagnostics for positive and negative checks.
def command_result(args, *arguments):
    return subprocess.run(
        args.wrapper + [args.executable, args.package_path, *arguments],
        encoding="utf-8",
        capture_output=True,
    )


# Return standard output for commands that are required to succeed.
def run_command(args, *arguments):
    result = command_result(args, *arguments)
    if result.returncode != 0:
        raise AssertionError(result.stdout + result.stderr)
    return result.stdout


def run_help(args, *topics):
    return run_command(args, "help", *topics)


def between(text, first, second):
    return text.split(first, 1)[1].split(second, 1)[0]


# Check exact topic lookup, including case-distinct function/alphabet names and slash markers.
# The slash-specific checks become obsolete if help topics acquire genuine path syntax.
def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--wrapper", action="append", default=[])
    parser.add_argument("executable")
    parser.add_argument("package_path")
    args = parser.parse_args()

    # Plain help cannot detect restrictions on CLI11 flag values or repetition.
    # Keep the native spellings usable; remove if help deliberately adopts a stricter syntax.
    plain_help = run_command(args, "--help")
    for arguments in (("--help=yes",), ("-h", "--help=true")):
        if run_command(args, *arguments) != plain_help:
            raise AssertionError("help flag values or repetition changed the help output")

    top_level = run_help(args)
    expected_basic_usage = (
        "bali-phy [OPTIONS] [INFER-OPTIONS] SEQUENCE-FILE [SEQUENCE-FILE ...]",
        "bali-phy [OPTIONS] help [TOPIC]",
    )
    for usage in expected_basic_usage:
        if usage not in top_level:
            raise AssertionError(f"top-level help omitted usage: {usage}")
    # Keep one blank line at each usage boundary; formatter components can otherwise each add one.
    if "\n\n\nUsage:\n" in top_level or "\n\nUsage:\n" not in top_level:
        raise AssertionError("top-level help has incorrect spacing before usage")
    usage_end = "bali-phy [OPTIONS] help [TOPIC]\n\nGlobal options:"
    if usage_end not in top_level:
        raise AssertionError("top-level help has incorrect spacing after usage")
    for unwanted in ("SUBCOMMAND", "Inference:", "Commands:", "POSITIONALS:"):
        if unwanted in top_level:
            raise AssertionError(f"top-level help retained the {unwanted} presentation")
    infer_options = between(top_level, "Infer options:", "Showing ")
    # Root usage already names positionals; retain their descriptions only in focused command help.
    if "SEQUENCE-FILE ..." in infer_options or "Help options:" in top_level:
        raise AssertionError("top-level help redundantly described positional arguments")
    if "--align" in infer_options:
        raise AssertionError("positional sequence files retained a named alias")
    if "--config FILE" not in infer_options:
        raise AssertionError("--config was not presented as an inference option")
    global_options = between(top_level, "Global options:", "Infer options:")
    if "--config" in global_options:
        raise AssertionError("--config was presented as a global option")
    if "models/" not in top_level:
        raise AssertionError("top-level help did not mark models as having subtopics")
    if "  * `bali-phy help " not in top_level:
        raise AssertionError("top-level help lost its indented help guidance")
    for example in ("alphabet", "Normal", "TN93", "log"):
        if f"help \x1b[1m{example}\x1b[0m" not in top_level:
            raise AssertionError(f"top-level help omitted or did not emphasize the {example} example")
    topic_line = next((line for line in top_level.splitlines() if "alphabets/" in line), "")
    if not topic_line.startswith("   ") or "   commands/" not in topic_line:
        raise AssertionError("top-level help lost topic indentation or column alignment")

    advanced = run_help(args, "advanced")
    if "Run options:" in advanced or "Help options:" in advanced:
        raise AssertionError("advanced help retained sections containing only positional arguments")
    # Cumulative help should present inherited fallthrough options only in the global section.
    # This guards against CLI11 version changes exposing parent options through subcommands.
    for option in ("--verbose LEVEL", "--seed SEED", "--version"):
        if advanced.count(option) != 1:
            raise AssertionError(f"advanced help did not present {option} exactly once")
    for usage in (
        "bali-phy [OPTIONS] run PROGRAM [ARGUMENT ...]",
        "bali-phy [OPTIONS] print [PRINT-OPTIONS] EXPRESSION",
    ):
        if usage not in advanced:
            raise AssertionError(f"advanced help omitted usage: {usage}")
    if "`bali-phy help` to see fewer options" not in advanced:
        raise AssertionError("advanced help did not point back to basic help")
    if "to see more options" not in advanced:
        raise AssertionError("advanced help did not point to the next help level")

    developer = run_help(args, "developer")
    for usage in (
        "bali-phy [OPTIONS] type NAME",
        "bali-phy [OPTIONS] test-module MODULE",
    ):
        if usage not in developer:
            raise AssertionError(f"developer help omitted usage: {usage}")

    for command, usage, positional in (
        ("run", "bali-phy [OPTIONS] run PROGRAM [ARGUMENT ...]", "ARGUMENT ..."),
        ("print", "bali-phy [OPTIONS] print [PRINT-OPTIONS] EXPRESSION", "EXPRESSION"),
    ):
        command_help = run_help(args, command)
        if usage not in command_help:
            raise AssertionError(f"direct {command} help omitted its semantic usage")
        if "POSITIONALS:" in command_help:
            raise AssertionError(f"direct {command} help separated positional arguments")
        if "Global options:" not in command_help or "--seed SEED" not in command_help:
            raise AssertionError(f"direct {command} help omitted applicable global options")
        if command_help.count("--seed SEED") != 1:
            raise AssertionError(f"direct {command} help repeated applicable global options")
        if f"{command.capitalize()} options:" not in command_help:
            raise AssertionError(f"direct {command} help omitted its combined option section")
        if positional not in command_help:
            raise AssertionError(f"direct {command} help omitted positional argument {positional}")

    # Both help entry points should use the same local/global option split for a fallthrough command.
    run_option_help = run_command(args, "run", "--help")
    if "Run options:" not in run_option_help or run_option_help.count("--seed SEED") != 1:
        raise AssertionError("run --help did not separate local and global options")

    # Inference is the unnamed default, so it must not remain addressable as a command-help topic;
    # mixed default/named inputs must fail before help dispatch can hide the conflict.
    infer_help = run_help(args, "infer")
    if "Help topic 'infer' not found." not in infer_help or "Infer options:" in infer_help:
        raise AssertionError("help infer still resolved as command help")
    for arguments in (("--smodel", "JC69", "help"), ("data.fasta", "help")):
        result = command_result(args, *arguments)
        diagnostics = result.stdout + result.stderr
        if result.returncode == 0 or "Inference inputs cannot be used with the help command" not in diagnostics:
            raise AssertionError("help accepted inference inputs instead of reporting their conflict")

    # Synthetic bindings isolate empty/default-only entries and operators without fixity, which
    # catalogue entries need not exercise. Retire if binding help stops using these JSON fields.
    with tempfile.TemporaryDirectory(dir=".") as temporary:
        bindings = Path(temporary) / "bindings"
        bindings.mkdir()
        entry = {"name": "%+%", "result_type": "a", "call": "@x",
                 "args": [{"name": "x", "type": "a"}, {"name": "y", "type": "a"}]}
        (bindings / "operator.json").write_text(json.dumps(entry))
        local_args = argparse.Namespace(**vars(args))
        # Wine uses Windows path-list separators; relative paths work in its mapped working directory.
        separator = ";" if args.wrapper or os.name == "nt" else ":"
        local_args.package_path = args.package_path + separator + temporary
        text = re.sub(r"\x1b\[[0-9;]*m", "", run_help(local_args, "%+%"))
        if "(x: a) %+% (y: a) -> a" not in text or "Arguments" in text:
            raise AssertionError("operator help required fixity or displayed empty argument details")
        entry["name"] = "helpDetailsTest"
        entry["args"][0]["description"] = "   "
        entry["args"][1]["default_value"] = "@x"
        entry["args"].append({"name": "z", "type": "a", "description": "An explanation."})
        (bindings / "details.json").write_text(json.dumps(entry))
        text = re.sub(r"\x1b\[[0-9;]*m", "", run_help(local_args, "helpDetailsTest"))
        details = text.split("Arguments", 1)[1]
        if ("default" not in details or "   x" in details or "y:" in details or
                "z: An explanation." not in details or "explanation.." in details):
            raise AssertionError("argument help lost defaults or retained empty/duplicate punctuation")

    models = run_help(args, "models")
    if "Covarion/" not in models:
        raise AssertionError("model help did not mark Covarion as having subtopics")
    if "GTR/" in models:
        raise AssertionError("model help marked the leaf topic GTR as having subtopics")

    functions = run_help(args, "functions")
    # Canonical-topic checks do not catch aliases that work in expressions but lack help.
    # Keep alias help identical while ordinary binding synonyms share one definition.
    for alias, canonical in (("density", "pdf"), ("UnitMixture", "unit_mixture")):
        if run_help(args, alias) != run_help(args, canonical):
            raise AssertionError(f"{alias} did not resolve to {canonical} help")
    for name in ("codons", "doublets", "triplets"):
        if name not in functions:
            raise AssertionError(f"function help did not list {name}")
    for old_name in ("Codons", "codonsOf", "doubletsOf", "tripletsOf"):
        if old_name in functions:
            raise AssertionError(f"function help still listed {old_name}")

    alphabets = run_help(args, "alphabets")
    for name in ("Codons", "Doublets", "Triplets"):
        if name not in alphabets:
            raise AssertionError(f"alphabet help did not list {name}")

    codons_function = run_help(args, "codons")
    codons_alphabet = run_help(args, "Codons")
    if "Usage" not in codons_function:
        raise AssertionError("codons did not resolve to function help")
    if "genetic-code" not in codons_alphabet:
        raise AssertionError("Codons did not resolve to alphabet help")
    if codons_function == codons_alphabet:
        raise AssertionError("codons and Codons resolved to the same help topic")

    if run_help(args, "models/") != models:
        raise AssertionError("models and models/ produced different help")

    run_help(args, "GTR")
    leaf_slash = run_help(args, "GTR/")
    if "Help topic 'GTR' has no subtopics." not in leaf_slash:
        raise AssertionError("GTR/ did not explain that GTR has no subtopics")

    division = run_help(args, "/")
    if "Usage" not in division:
        raise AssertionError("the division operator was mistaken for a subtopic marker")


if __name__ == "__main__":
    main()
