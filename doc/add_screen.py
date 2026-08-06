#!/usr/bin/env python3

import fileinput
import re
import sys


PROMPT_WITH_COMMENT = re.compile(r"^% (.*)# (.*)$")
NO_PROMPT = re.compile(r"^%%(.*)$")
PROMPT = re.compile(r"^% (.*)$")
CONTINUATION = re.compile(r"^\| (.*)$")


def replace_match(line, match, replacement):
    """Replace a match while retaining a trailing newline excluded by `$`."""
    return replacement + line[match.end():]


def screen_line(line):
    match = PROMPT_WITH_COMMENT.match(line)
    if match:
        replacement = (
            f"<prompt>%</prompt> <userinput>{match.group(1)}</userinput>"
            f"# {match.group(2)}"
        )
        return replace_match(line, match, replacement)

    match = NO_PROMPT.match(line)
    if match:
        return replace_match(line, match, match.group(1))

    match = PROMPT.match(line)
    if match:
        replacement = f"<prompt>%</prompt> <userinput>{match.group(1)}</userinput>"
        return replace_match(line, match, replacement)

    if CONTINUATION.match(line):
        return line

    return None


def main():
    pending_screen = []
    for line in fileinput.input(encoding="utf-8"):
        transformed = screen_line(line)
        if transformed is not None:
            pending_screen.append(transformed)
            continue

        if pending_screen:
            sys.stdout.write(f"<screen>{''.join(pending_screen)}</screen>")
            pending_screen = []
        sys.stdout.write(line)

    # Preserve the legacy transform: a screen is emitted only when a later ordinary line closes it.


if __name__ == "__main__":
    main()
