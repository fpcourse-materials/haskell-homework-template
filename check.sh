#!/usr/bin/env sh
set -e
hlint src test
cabal build all
cabal test homework-test --test-show-details=direct --test-options="$1 $(cat solved-tasks.txt)"
