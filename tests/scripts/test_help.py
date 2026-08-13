#!/usr/bin/env python3

import argparse
import subprocess


# Run a help request and return its standard output, failing on execution errors.
def run_help(args, *topics):
    result = subprocess.run(
        args.wrapper + [args.executable, args.package_path, "help", *topics],
        text=True,
        capture_output=True,
    )
    if result.returncode != 0:
        raise AssertionError(result.stdout + result.stderr)
    return result.stdout


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

    top_level = run_help(args)
    expected_basic_usage = (
        "bali-phy [OPTIONS] infer [INFER-OPTIONS] SEQUENCE-FILE [SEQUENCE-FILE ...]",
        "bali-phy [OPTIONS] help [TOPIC]",
    )
    for usage in expected_basic_usage:
        if usage not in top_level:
            raise AssertionError(f"top-level help omitted usage: {usage}")
    for unwanted in ("SUBCOMMAND", "Inference:", "Commands:", "POSITIONALS:"):
        if unwanted in top_level:
            raise AssertionError(f"top-level help retained the {unwanted} presentation")
    infer_options = between(top_level, "Infer options:", "Help options:")
    if "SEQUENCE-FILE ..." not in infer_options or "SEQUENCE-FILE TEXT" in infer_options:
        raise AssertionError("inference help did not use a semantic sequence-file metavariable")
    if "--align SEQUENCE-FILE" not in infer_options:
        raise AssertionError("--align did not use the sequence-file metavariable")
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

    for command, usage in (
        ("infer", expected_basic_usage[0]),
        ("run", "bali-phy [OPTIONS] run PROGRAM [ARGUMENT ...]"),
        ("print", "bali-phy [OPTIONS] print [PRINT-OPTIONS] EXPRESSION"),
    ):
        command_help = run_help(args, command)
        if usage not in command_help:
            raise AssertionError(f"direct {command} help omitted its semantic usage")
        if "POSITIONALS:" in command_help:
            raise AssertionError(f"direct {command} help separated positional arguments")
        if "Global options:" not in command_help or "--seed SEED" not in command_help:
            raise AssertionError(f"direct {command} help omitted applicable global options")
        if f"{command.capitalize()} options:" not in command_help:
            raise AssertionError(f"direct {command} help omitted its combined option section")

    models = run_help(args, "models")
    if "Covarion/" not in models:
        raise AssertionError("model help did not mark Covarion as having subtopics")
    if "GTR/" in models:
        raise AssertionError("model help marked the leaf topic GTR as having subtopics")

    functions = run_help(args, "functions")
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
