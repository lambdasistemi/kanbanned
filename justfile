# shellcheck shell=bash

set unstable := true

# List available recipes
default:
    @just --list

# Format all source files
format:
    #!/usr/bin/env bash
    set -euo pipefail
    for i in {1..3}; do
        fourmolu -i src app test
    done
    cabal-fmt -i *.cabal
    nixfmt *.nix nix/*.nix

# Check formatting (for CI)
format-check:
    #!/usr/bin/env bash
    set -euo pipefail
    fourmolu -m check src app test
    cabal-fmt -c *.cabal
    nixfmt -c *.nix nix/*.nix

# Run hlint
hlint:
    #!/usr/bin/env bash
    hlint src app test

# Build all components
build:
    #!/usr/bin/env bash
    cabal build all --enable-tests -O0

# Run unit tests with optional match pattern
unit match="":
    #!/usr/bin/env bash
    if [[ '{{ match }}' == "" ]]; then
        cabal test unit-tests -O0 --test-show-details=direct
    else
        cabal test unit-tests -O0 \
            --test-show-details=direct \
            --test-option=--match \
            --test-option="{{ match }}"
    fi

# Check cabal package
cabal-check:
    cabal check --ignore=missing-upper-bounds --ignore=option-o2

# Full CI pipeline
ci:
    #!/usr/bin/env bash
    set -euo pipefail
    just build
    just unit
    just format-check
    just hlint
    just cabal-check
