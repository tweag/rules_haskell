# GMP

When building on NixOS or with the platform `..._nixpkgs` gmp is fetched from
nixpkgs. On other platforms we have no guarantees that GMP is present. On Nix
platforms we try to fetch it from nixpkgs. However on non-Nix platforms `@gmp`
needs to be defined nontheless (so far on all non-Nix platforms Bazel manages
to find libgmp) so we create a dummy target with an empty header file. Note
that this target cannot be completely empty: Bazel doesn't accept empty
libraries.
