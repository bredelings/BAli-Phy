#!/usr/bin/env python3

import argparse
from pathlib import Path
import re


# Convert named UTF-8 text assets into constexpr C++ raw-string declarations.
def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("output", type=Path)
    parser.add_argument("bindings", nargs="+")
    args = parser.parse_args()

    if len(args.bindings) % 2:
        parser.error("bindings must be NAME FILE pairs")

    declarations = []
    for binding in range(0, len(args.bindings), 2):
        name = args.bindings[binding]
        filename = Path(args.bindings[binding + 1])
        if not re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", name):
            parser.error(f"invalid C++ identifier: {name}")

        contents = filename.read_text(encoding="utf-8")
        delimiter = f"BALIPHY_ASSET_{binding // 2}"
        while f'){delimiter}"' in contents:
            delimiter += "_"
        declarations.append(
            f'inline constexpr char {name}[] = '
            f'R"{delimiter}({contents}){delimiter}";\n'
        )

    header = "#pragma once\n\n" + "\n".join(declarations)
    args.output.write_text(header, encoding="utf-8")


if __name__ == "__main__":
    main()
