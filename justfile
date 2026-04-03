nix := "bash scripts/in-nix.sh"

[private]
default:
    @just --list

# Compile the project
build:
    {{nix}} dune build

# Recompile and run tests on file changes
watch:
    {{nix}} dune build @all @runtest --watch

# Run chasity with arbitrary arguments, e.g. `just run generate --shapes foo.ttl`
run *ARGS:
    {{nix}} dune exec bin/main.exe -- {{ARGS}}

# Run all tests in test/ (--force to re-run cached results)
test *ARGS:
    {{nix}} dune runtest {{ARGS}}

# Delete _build/ artifacts
clean:
    {{nix}} dune clean

# Auto-format all OCaml sources
format:
    {{nix}} dune fmt

# Build the nix package and run a command, e.g. `just nix-run --version`
nix-run *ARGS:
    nix run .# -- {{ARGS}}

# Enter the dev environment with all tools (ocamllsp, dune, riot, etc.)
dev:
    bash scripts/dev-shell.sh

# Drop into a temporary shell with chasity on PATH (exit to leave)
nix-shell:
    nix shell .#

# Validate test fixtures against SHACL shapes
validate-fixtures:
    {{nix}} bash scripts/validate-fixtures.sh

# Generate test protos from fixtures into test/output/
test-generate:
    mkdir -p test/output
    printf 'version: v2\ndeps:\n  - buf.build/bufbuild/protovalidate\n' > test/output/buf.yaml
    {{nix}} dune exec bin/main.exe -- generate --shapes test/fixtures --out test/output --package test.v1

# Generate test protos then run buf lint
test-lint: test-generate
    {{nix}} bash -c 'cd test/output && buf dep update && buf lint --path test/v1/'

# Print the CLI help text
help:
    {{nix}} dune exec chasity -- --help
