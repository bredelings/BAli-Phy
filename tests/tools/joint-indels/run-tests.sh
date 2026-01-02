#!/bin/bash
# Run joint-indels unit tests
# Usage: ./run-tests.sh [path-to-joint-indels-binary]

# Convert to absolute path if relative
if [[ "${1:-}" = /* ]]; then
    JOINT_INDELS="$1"
elif [[ -n "${1:-}" ]]; then
    JOINT_INDELS="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
else
    JOINT_INDELS="joint-indels"
fi

# Use MESON_EXE_WRAPPER if set (needed for cross-compilation, e.g., Wine on Windows)
if [[ -n "${MESON_EXE_WRAPPER:-}" ]]; then
    JOINT_INDELS="$MESON_EXE_WRAPPER $JOINT_INDELS"
fi
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
FAILED=0
PASSED=0

echo "Testing joint-indels --details output format"
echo "============================================="
echo ""

for example in example1 example2 example3; do
    cd "$SCRIPT_DIR/$example"

    # Read command args from file (normalize line endings for Windows)
    CMD=$(cat args.txt | tr -d '\r')

    # Run joint-indels (normalize line endings for Windows compatibility)
    # Note: $JOINT_INDELS is unquoted to allow word splitting when MESON_EXE_WRAPPER is set
    # Stderr is discarded to avoid Wine warnings polluting the output
    OUTPUT=$($JOINT_INDELS $CMD 2>/dev/null | tr -d '\r')
    EXPECTED=$(cat expected-output | tr -d '\r')

    if [ "$OUTPUT" = "$EXPECTED" ]; then
        echo "✓ $example: PASSED"
        ((PASSED++))
    else
        echo "✗ $example: FAILED"
        echo "  Expected:"
        echo "$EXPECTED" | sed 's/^/    /'
        echo "  Got:"
        echo "$OUTPUT" | sed 's/^/    /'
        ((FAILED++))
    fi
done

echo ""
echo "============================================="
echo "Results: $PASSED passed, $FAILED failed"

exit $FAILED
