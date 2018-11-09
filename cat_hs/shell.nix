{ }:

with (import ./nix {});

mkShell {
  nativeBuildInputs = [ bazel nix.out perl python.out ];
}
