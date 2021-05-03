#!/usr/bin/env bash

[ "$DEBUG" = "true" ] && set -x
set -euo pipefail

PROJECT_DIR=${PROJECT_DIR:-"$(cd "$(dirname "$(dirname "${BASH_SOURCE[0]}")")" && pwd)"}
CANDIDATE="${CANDIDATE:-"true"}"

cd "$PROJECT_DIR"

if [ -z "$*" ]; then
    echo "Some items is needed." >&2
    exit 1
fi

if ! git branch | grep '^* master$' >/dev/null; then
    echo "Not on master branch" >&2
    exit 1
fi

for item in "$@"; do
    echo "Publishing $item"

    target="$(echo "$item" | sed 's/-[0-9.]*$//')"
    cabal sdist "$target"

    dist_file="$PROJECT_DIR/dist-newstyle/sdist/$item.tar.gz"
    if [ ! -e "$dist_file" ]; then
        echo "$item is not exists." >&2
        exit 1
    fi

    if [ "$CANDIDATE" = "true" ]; then
        cabal upload "$dist_file"
    else
        cabal upload --publish "$dist_file"
        git tag "$item"
        git push origin "$item"
    fi
done
