#!/usr/bin/env bash
# Validates test fixtures against SHACL shapes.
# Usage: bash scripts/validate-fixtures.sh

set -euo pipefail

check() {
    local label="$1" expected="$2"
    shift 2
    if "$@" | grep -q "sh:conforms  $expected"; then
        echo "PASS: $label"
    else
        echo "FAIL: $label"
        exit 1
    fi
}

check "person_good.ttl conforms" true \
    shacl validate --shapes test/fixtures/person.ttl \
    --data test/fixtures/data/person_good.ttl \
    --data test/fixtures/data/organization_good.ttl

check "person_bad.ttl rejected" false \
    shacl validate --shapes test/fixtures/person.ttl \
    --data test/fixtures/data/person_bad.ttl
