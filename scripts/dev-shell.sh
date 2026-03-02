#!/usr/bin/env bash

# Enter nix dev shell if not already inside one.
# Usage: bash scripts/dev-shell.sh

if [ -n "$IN_NIX_SHELL" ]; then
    echo "Already in nix dev shell."
    exec "$SHELL"
else
    exec nix develop
fi
