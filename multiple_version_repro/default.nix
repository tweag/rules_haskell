with import ./nixpkgs.nix {};
mkShell {
  buildInputs = [ bazel ];
}
