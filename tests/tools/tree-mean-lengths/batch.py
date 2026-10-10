#!/usr/bin/env python3

import argparse
import os
from pathlib import Path
import subprocess
import tempfile


# Run the real command with literal paths, capturing failures as well as successful output.
def run(command, *args, stdin=None, code=0):
    result = subprocess.run(command + list(args), input=stdin, capture_output=True, text=True)
    assert result.returncode == code, (args, result.returncode, result.stdout, result.stderr)
    return result.stdout, result.stderr


# Batch equivalence, failure isolation, and untouched destinations require multiple files;
# the single-output tests cannot exercise these contracts. Retain while named batches exist.
def check_batch(command, wrappers):
    Path("query.tree").write_text("((A:1,B:1):1,C:1,D:1);\n")
    Path("query, star.tree").write_text("(D:1,C:1,B:1,A:1);\n")
    samples = "((A:1,B:2):3,C:4,D:5);\n((A:3,B:4):5,C:6,D:7);\n"
    Path("samples.trees").write_text(samples)
    pairs = ["--trees", "query.tree", "binary.out", "--trees", "query, star.tree", "star, output.out"]
    for mode in [[], ["--show-node-lengths"], ["--simple"], ["--var"], ["--no-node-lengths"]]:
        expected = [run(command, "--tree", q, *mode, "samples.trees")[0]
                    for q in ["query.tree", "query, star.tree"]]
        assert run(command, *pairs, *mode, "-", stdin=samples) == ("", "")
        assert Path("binary.out").read_text() == expected[0]
        assert Path("star, output.out").read_text() == expected[1]
    assert run(command, *pairs[:-1], "-", "--show-node-lengths", "samples.trees")[0] == run(
        command, "--tree", "query, star.tree", "--show-node-lengths", "samples.trees")[0]

    # Batch filtering must match separate invocations, including restarting filters for each file.
    lines = samples.splitlines(keepends=True)
    for name in ["one.trees", "two.trees"]:
        Path(name).write_text(lines[0] + lines[0] + lines[1] + lines[0])
    filters = ["--skip=1", "--subsample=2", "--until=3"]
    run(command, *pairs, "--simple", *filters, "one.trees", "two.trees")
    for q, out in [("query.tree", "binary.out"), ("query, star.tree", "star, output.out")]:
        assert Path(out).read_text() == run(command, "--tree", q, "--simple", *filters,
                                           "one.trees", "two.trees")[0]

    # First query fails; the later star must still be written and any old failed output preserved.
    Path("conflict.trees").write_text("(A:1,C:1,(B:1,D:1):2);\n")
    Path("binary.out").write_text("keep this output\n")
    out, error = run(command, *pairs, "--show-node-lengths", "conflict.trees", code=1)
    assert out == "" and "Query 'query.tree'" in error
    assert Path("binary.out").read_text() == "keep this output\n"
    assert Path("star, output.out").read_text() == run(
        command, "--tree", "query, star.tree", "--show-node-lengths", "conflict.trees")[0]
    run(command, *pairs, "--safe", "--show-node-lengths", "conflict.trees")
    assert Path("binary.out").read_text() == "(A,B,(C,D));\n"

    # A late parse error invalidates partial estimates even under --safe, before files are opened.
    Path("broken.trees").write_text(samples + "(A:bad,B:1,C:1,D:1);\n")
    for safe in [[], ["--safe"]]:
        Path("binary.out").write_text("keep binary\n")
        Path("star, output.out").write_text("keep star\n")
        assert "Failed to read sample file" in run(command, *pairs, *safe, "broken.trees", code=1)[1]
        assert Path("binary.out").read_text() == "keep binary\n"
        assert Path("star, output.out").read_text() == "keep star\n"

    # A failed destination must not prevent later output; setup errors must precede all writes.
    run(command, "--trees", "query.tree", "absent/out", *pairs[3:], "samples.trees", code=1)
    assert Path("star, output.out").read_text() != "keep star\n"
    Path("different.tree").write_text("(A,B,C,E);\n")
    cases = [
        ["--trees", "query.tree", "binary.out", "--trees", "different.tree", "other.out"],
        ["--trees", "query.tree", "binary.out", "--trees", "query.tree", "./binary.out"],
        ["--trees", "query.tree", "query.tree"],
        ["--trees", "query.tree", "samples.trees"],
        ["--trees", "query.tree", "-", "--trees", "query.tree", "-"],
        ["--tree", "query.tree", "--trees", "query.tree", "binary.out"],
    ]
    for args in cases:
        assert run(command, *args, "samples.trees", code=1)[0] == ""
        assert Path("binary.out").read_text() == "keep binary\n"
        assert Path("samples.trees").read_text() == samples
    run(command, "--trees", "query.tree", code=1)
    # Colons are literal on filesystems that permit them; Wine uses Windows filename rules.
    if not wrappers and os.name != "nt":
        Path("query:name.tree").write_text(Path("query.tree").read_text())
        run(command, "--trees", "query:name.tree", "output:name.tree", "samples.trees")
        assert Path("output:name.tree").read_text() == run(command, "--tree", "query.tree", "samples.trees")[0]


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--wrapper", action="append", default=[])
    parser.add_argument("binary", type=Path)
    args = parser.parse_args()
    command = args.wrapper + [str(args.binary.resolve())]
    with tempfile.TemporaryDirectory() as directory:
        os.chdir(directory)
        check_batch(command, args.wrapper)
