With bzlmod, the [nix functionalities](https://api.haskell.build/haskell/nixpkgs.html) provided by rules_haskell must be 
accessed via `@rules_haskell_nix//:nixpkgs.bzl`.

This way the main `rules_haskell` module does not have to depend on
`rules_nixpkgs` when we do not intend to use nix toolchains.
