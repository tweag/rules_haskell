{ pkgs ? import ./nixpkgs {}, docTools ? true }:

with pkgs;
mkShell {
  # Set UTF-8 local so that run-tests can parse GHC's unicode output.
  LANG="C.UTF-8";

  buildInputs = [
    go
    nix
    which
    perl
    python3
    bazel_4
    jdk11
    # For stack_install.
    stack
    # Needed for @com_github_golang_protobuf, itself needed by buildifier.
    git
    # Needed to get correct locale for tests with encoding
    glibcLocales
    # to avoid CA certificate failures on macOS CI
    cacert
    # Needed for debug/linking_utils
    binutils
    # check the start script for problems
    shellcheck
    file
  ] ++ lib.optionals docTools [graphviz python37Packages.sphinx zip unzip];

  shellHook = ''
    # Add nix config flags to .bazelrc.local.
    #
    BAZELRC_LOCAL=".bazelrc.local"
    if [ ! -e "$BAZELRC_LOCAL" ]
    then
      echo "[!] It looks like you are using a Nix-based system."
      echo "In order to build this project, you need to add the two"
      echo "following host_platform entries to your .bazelrc.local file:"
      echo
      echo "build --host_platform=@io_tweag_rules_nixpkgs//nixpkgs/platforms:host"
      echo "run --host_platform=@io_tweag_rules_nixpkgs//nixpkgs/platforms:host"
    fi

    # source bazel bash completion
    source ${bazel_4}/share/bash-completion/completions/bazel.bash
  '';
}
