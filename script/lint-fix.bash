#!/usr/bin/env bash

[ "$DEBUG" = "true" ] && set -x
set -euo pipefail

PROJECT_DIR=${PROJECT_DIR:-"$(cd "$(dirname "$(dirname "${BASH_SOURCE[0]}")")" && pwd)"}

for cabal_path in $(find "${PROJECT_DIR}" -name '*.cabal'); do
    (
        cd "$(dirname "$cabal_path")" &&
        stylish-haskell -i $(find . -name '*.hs')
    )
done
