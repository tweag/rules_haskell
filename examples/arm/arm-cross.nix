let
  crossPkgs = pkgs.pkgsCross.aarch64-multiplatform;
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/b90fbaa272a6d17ddc21164ca3056e1618feafcd.tar.gz) { };
  pkgs = import haskellNix.sources.nixpkgs haskellNix.nixpkgsArgs;
  ghc-iserv = "${crossGHC}/lib/${crossGHC.targetPrefix}ghc-${crossGHC.version}/bin/ghc-iserv";
  crossNumactl = crossPkgs.numactl;
  qemu = pkgs.buildPackages.qemu;
  qemuIservWrapper = pkgs.writeScriptBin "iserv-wrapper" ''
    #!${pkgs.stdenv.shell}
    set -euo pipefail
    # Unset configure flags as configure should have run already
    unset configureFlags
    exec ${qemu}/bin/qemu-aarch64 ${ghc-iserv} "$@"
  '';

  crossGHCLLVMWrapper = pkgs.writeScriptBin "ghc-llvm-wrapper" ''
    #!${pkgs.stdenv.shell}
    set -euo pipefail
    PATH="${pkgs.llvm_9}/bin:''${PATH:-}" ${crossGHC}/bin/aarch64-unknown-linux-gnu-ghc -pgmi ${qemuIservWrapper}/bin/iserv-wrapper -fexternal-interpreter -optl-L${crossNumactl}/lib "$@"
  '';

  crossGHC = crossPkgs.buildPackages.haskell-nix.compiler.ghc925;
  crossGCC = crossPkgs.buildPackages.gcc;
  crossGCCUnwrapped = crossPkgs.buildPackages.gcc-unwrapped;
  crossBinutils = crossPkgs.buildPackages.binutils;
  crossBinutilsUnwrapped = crossPkgs.buildPackages.binutils-unwrapped;

  prefixStrippedGHC = pkgs.runCommand "ghc-aarch64-symlinks" { }
    ''
      mkdir -p $out/bin
      for tool in \
        ghc-9.2.5 \
        ghc-pkg \
        ghc-pkg-9.2.5 \
        ghci \
        ghci-9.2.5 \
        hp2ps \
        hpc \
        hsc2hs \
        runghc \
        runghc-9.2.5 \
        runhaskell
      do
          ln -s ${crossGHC}/bin/aarch64-unknown-linux-gnu-$tool $out/bin/$tool
      done;
      mkdir -p $out/lib
      ln -s ${crossGHC}/lib/aarch64-unknown-linux-gnu-ghc-9.2.5 $out/lib/ghc-9.2.5
      ln -s ${crossGHCLLVMWrapper}/bin/ghc-llvm-wrapper $out/bin/ghc
      touch $out/bin/haddock
    '';

  prefixStrippedGCC = pkgs.runCommand "gcc-aarch64-symlinks" { } ''
    mkdir -p $out/bin
    for tool in \
      ar \
      dwp \
      nm \
      objcopy \
      objdump \
      strip
    do
        ln -s ${crossBinutilsUnwrapped}/bin/aarch64-unknown-linux-gnu-$tool $out/bin/$tool
    done;
    ln -s ${crossBinutils}/bin/aarch64-unknown-linux-gnu-ld $out/bin/ld
    for tool in \
      cc \
      gcov
    do
        ln -s ${crossGCC}/bin/aarch64-unknown-linux-gnu-$tool $out/bin/$tool
    done;
    ln -s ${crossGCCUnwrapped}/bin/aarch64-unknown-linux-gnu-cpp $out/bin/cpp
  '';

in
{
  ghc-aarch64 = pkgs.buildEnv {
    name = "ghc-aarch64-env";
    paths =
      [
        prefixStrippedGHC
        qemuIservWrapper
        crossGHCLLVMWrapper
      ];
  };
  cc-aarch64 = pkgs.buildEnv {
    name = "cc-aarch64-env";
    passthru = { isClang = false; };
    paths =
      [
        prefixStrippedGCC
      ];
  };
  inherit pkgs;
}
