#!/usr/bin/env python3

import argparse
import subprocess


# Run a help request and return its standard output, failing on execution errors.
def run_help(args, *topics):
    result = subprocess.run(
        args.wrapper + [args.executable, "help", *topics, args.package_path],
        text=True,
        capture_output=True,
    )
    if result.returncode != 0:
        raise AssertionError(result.stdout + result.stderr)
    return result.stdout


# Check exact topic lookup, including case-distinct function/alphabet names and slash markers.
# The slash-specific checks become obsolete if help topics acquire genuine path syntax.
def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--wrapper", action="append", default=[])
    parser.add_argument("executable")
    parser.add_argument("package_path")
    args = parser.parse_args()

    top_level = run_help(args)
    if "models/" not in top_level:
        raise AssertionError("top-level help did not mark models as having subtopics")

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
