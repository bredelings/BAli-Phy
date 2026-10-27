from pathlib import Path
import sys


# The overview must expose each report command and its help entry point.
# Numerical report tests do not cover discovery; retain while the commands are separate.
help_text = (Path(sys.argv[1]) / "output").read_text(encoding="utf-8")
assert "summarize" in help_text
assert "columns" in help_text
assert "letters" in help_text
assert "positive-selection" in help_text
assert "COMMAND --help" in help_text
