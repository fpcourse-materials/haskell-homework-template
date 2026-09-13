#!/usr/bin/env sh
set -e
hlint src test
cabal build all --ghc-options=-Werror
