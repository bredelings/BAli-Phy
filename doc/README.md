# BAli-Phy documentation

| Document | Source | Purpose |
| --- | --- | --- |
| User guide | [UserGuide.xml](UserGuide.xml) | Installation, analyses, models, and command-line tools |
| Tutorial | [Tutorial.tut.xml](Tutorial.tut.xml) | Worked analyses with example data |
| Instructions | [Instructions.tut.xml](Instructions.tut.xml) | Exercises for hands-on sessions |
| Man pages | [man/](man/) | Command reference, built and installed by Meson |
| Function reference | JSON files in [bindings/](../bindings/) | Model and function descriptions rendered by `generate-reference.py` |
| Developer documentation | [developer/](developer/) | Software architecture, development, and distribution |

See [AUTHORING.md](AUTHORING.md) for writing conventions and markup examples, and
[BUILDING.md](BUILDING.md) for dependencies, configuration, rendering, and publishing.

From the checkout root, after installing the dependencies:

```sh
make -C doc setup-user-guide
make -C doc all model-reference
make -C doc check-user-guide
```

Generated guide, tutorial, exercise, and reference files go under `doc/_build/`. Downloaded
build dependencies go under `doc/_tools/`. Both directories are ignored. Edit sources rather
than generated files. `make -C doc clean` removes those outputs but retains downloaded tools.
The developer documentation has its own Makefile; man pages use the software build directory.
