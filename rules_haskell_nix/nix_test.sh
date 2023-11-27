#!/usr/bin/env bash

export XDG_CACHE_HOME=${TMPDIR}

exec nix run -I nixpkgs=../nixpkgs 'nixpkgs#cowsay' -- hello, rules_haskell_nix
