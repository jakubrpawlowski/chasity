#!/usr/bin/env bash
# Validates test fixtures against SHACL shapes.
# Usage: bash scripts/validate-fixtures.sh

set -euo pipefail

check() {
    local label="$1" shapes="$2" data="$3" expected="$4"
    if shacl validate --shapes "$shapes" --data "$data" | grep -q "sh:conforms  $expected"; then
        echo "PASS: $label"
    else
        echo "FAIL: $label"
        exit 1
    fi
}

check "person_good.ttl conforms"    test/fixtures/person.ttl test/fixtures/person_good.ttl true
check "person_bad.ttl rejected"     test/fixtures/person.ttl test/fixtures/person_bad.ttl  false
