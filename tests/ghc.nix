{ pkgs ? import ../nixpkgs {}
# Whether we want to wrap the packages using <bazel_haskell_wrapper>.
# When this is called from inside bazel, we need to wrap the haskell package
# set using <bazel_haskell_wrapper>. Otherwise we don't need (and don't want)
# to.
, wrapPackages ? (builtins.tryEval <bazel_haskell_wrapper>).success
}:

with pkgs;

let haskellPackages = pkgs.haskell.packages.ghc864.override {
      overrides = with pkgs.haskell.lib; self: super: rec {
        libc = import ./haddock/libC.nix self pkgs;
      };
    };
    genBazelBuild =
      if wrapPackages
      then callPackage <bazel_haskell_wrapper> {}
      else (x: x);

in
  {
  ghc = haskellPackages.ghcWithPackages (p: with p; [

    # haskell_proto_library inputs
    bytestring
    containers
    data-default-class
    lens-family
    lens-labels
    proto-lens
    text

  # test inputs
  libc

  # for test runner
  hspec
  ]);
  haskellPackages = genBazelBuild haskellPackages;
}
