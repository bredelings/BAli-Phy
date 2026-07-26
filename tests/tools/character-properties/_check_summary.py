import json
from pathlib import Path


# Decode the command's captured JSON result for a case-specific structural check.
def read_summary(results):
    return json.loads((Path(results) / "output").read_text(encoding="utf-8"))


# Fail a structural check with both values visible in the test diagnostic.
def require_equal(obtained, expected):
    if obtained != expected:
        raise AssertionError(f"expected {expected!r}, obtained {obtained!r}")
