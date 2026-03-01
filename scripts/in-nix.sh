#!/usr/bin/env bash

# Run a command in nix dev shell if not already inside one.
# Usage: bash scripts/in-nix.sh <command> [args...]

if [ -n "$IN_NIX_SHELL" ]; then
    exec "$@"
else
    exec nix develop -c "$@"
fi
