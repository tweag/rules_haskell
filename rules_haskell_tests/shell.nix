{ pkgs ? import ./nixpkgs { }, docTools ? true, ghcVersion ? "9.4.8" }:

with pkgs;
mkShell {
  TMPDIR = "/tmp";

  GHC_VERSION = ghcVersion;

  # Set UTF-8 local so that run-tests can parse GHC's unicode output.
  LANG = "C.UTF-8";

  buildInputs = [
    go
    nix
    which
    perl
    python3
    jdk11
    # For stack_install.
    stack
    # Needed for ghcide which expects ghc in PATH.
    haskell.packages."ghc${ builtins.replaceStrings [ "." ] [ "" ] ghcVersion }".ghc
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
  ] ++ lib.optionals docTools [ graphviz python3Packages.sphinx zip unzip ];

  packages = [ bazel_7 ];

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
      echo "build --host_platform=@rules_nixpkgs_core//platforms:host"
      echo "run --host_platform=@rules_nixpkgs_core//platforms:host"
    fi
  '';
}
