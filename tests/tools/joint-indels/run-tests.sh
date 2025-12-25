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
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
FAILED=0
PASSED=0

echo "Testing joint-indels --details output format"
echo "============================================="
echo ""

for example in example1 example2 example3; do
    cd "$SCRIPT_DIR/$example"

    # Read command args from file
    CMD=$(cat args.txt)

    # Run joint-indels
    OUTPUT=$("$JOINT_INDELS" $CMD 2>&1)
    EXPECTED=$(cat expected-output)

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
