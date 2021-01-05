let crossPkgs = pkgs.pkgsCross.aarch64-multiplatform;
    haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/b406bb1d5bed581f651ae18a5d3ed07f47ace2b9.tar.gz) {};
    pkgs = import ./nixpkgs.nix haskellNix.nixpkgsArgs;
    iserv-proxy = pkgs.buildPackages.ghc-extra-packages.ghc8102.iserv-proxy.components.exes.iserv-proxy;
    remote-iserv = crossPkgs.ghc-extra-packages.ghc8102.remote-iserv.components.exes.remote-iserv;
    qemu = pkgs.buildPackages.qemu;
    qemuIservWrapper = pkgs.writeScriptBin "iserv-wrapper" ''
      #!${pkgs.stdenv.shell}
      set -euo pipefail
      # Unset configure flags as configure should have run already
      unset configureFlags
      PORT=$((5000 + $RANDOM % 5000))
      (>&2 echo "---> Starting remote-iserv on port $PORT")
      ${qemu}/bin/qemu-aarch64 ${remote-iserv}/bin/remote-iserv tmp $PORT &
      (>&2 echo "---| remote-iserv should have started on $PORT")
      RISERV_PID="$!"
      ${iserv-proxy}/bin/iserv-proxy $@ 127.0.0.1 "$PORT"
      (>&2 echo "---> killing remote-iserve...")
      kill $RISERV_PID
      '';
    
    crossGHC = crossPkgs.buildPackages.haskell-nix.compiler.ghc8102;
    crossGCC = crossPkgs.buildPackages.gcc;
    crossGCCUnwrapped = crossPkgs.buildPackages.gcc-unwrapped;
    crossBinutils = crossPkgs.buildPackages.binutils;
    crossBinutilsUnwrapped = crossPkgs.buildPackages.binutils-unwrapped;

    prefixStrippedGHC = pkgs.runCommand "ghc-aarch64-symlinks" {} ''
      mkdir -p $out/bin
      for tool in \
        ghc \
        ghc-8.10.2 \
        ghc-pkg \
        ghc-pkg-8.10.2 \
        ghci \
        ghci-8.10.2 \
        hp2ps \
        hpc \
        hsc2hs \
        runghc \
        runghc-8.10.2 \
        runhaskell
      do
          ln -s ${crossGHC}/bin/aarch64-unknown-linux-gnu-$tool $out/bin/$tool
      done;
      mkdir -p $out/lib
      ln -s ${crossGHC}/lib/aarch64-unknown-linux-gnu-ghc-8.10.2 $out/lib/ghc-8.10.2
      touch $out/bin/haddock
      '';

    prefixStrippedGCC = pkgs.runCommand "gcc-aarch64-symlinks" {} ''
	  echo prefixStrippedGCC $out/bin
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
{ ghc-aarch64 = pkgs.buildEnv {
    name = "ghc-aarch64-env";
    paths =
      [ prefixStrippedGHC
        qemuIservWrapper
      ];
  };
  cc-aarch64 = pkgs.buildEnv {
    name = "cc-aarch64-env";
    paths =
      [ prefixStrippedGCC
      ];
  };
}
