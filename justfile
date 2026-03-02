nix := "bash scripts/in-nix.sh"

[private]
default:
    @just --list

# Compile the project
build:
    {{nix}} dune build

# Run chasity with arbitrary arguments, e.g. `just run generate --shapes foo.ttl`
run *ARGS:
    {{nix}} dune exec chasity -- {{ARGS}}

# Run all tests in test/
test:
    {{nix}} dune runtest

# Delete _build/ artifacts
clean:
    {{nix}} dune clean

# Auto-format all OCaml sources
format:
    {{nix}} dune fmt

# Build the nix package and run a command, e.g. `just nix-run --version`
nix-run *ARGS:
    nix run .# -- {{ARGS}}

# Drop into a temporary shell with chasity on PATH (exit to leave)
nix-shell:
    nix shell .#

# Validate test fixtures against SHACL shapes
validate-fixtures:
    {{nix}} bash scripts/validate-fixtures.sh

# Print the CLI help text
help:
    {{nix}} dune exec chasity -- --help
