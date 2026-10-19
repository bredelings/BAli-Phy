#!/bin/sh

# Git Bash needs a shell launcher; Windows Python may be available as py or python.
# Keep interpreter selection consistent with the adjacent .cmd launcher.
script="$(dirname "$0")/bpy-summarize.py"
if command -v py >/dev/null 2>&1; then
    exec py -3 "$script" "$@"
else
    exec python "$script" "$@"
fi
