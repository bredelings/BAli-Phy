from pathlib import Path
import sys


# The overview is the durable discovery path for the split report commands and
# their required-file examples; subcommand smoke tests do not inspect its content.
help_text = (Path(sys.argv[1]) / "output").read_text(encoding="utf-8")
assert "summarize" in help_text
assert "report" in help_text
assert "positive-selection" in help_text
assert "P1.site-property-summary.json P1.initial.fasta rate" in help_text
