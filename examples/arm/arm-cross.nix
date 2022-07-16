let crossPkgs = pkgs.pkgsCross.aarch64-multiplatform;
    haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/0d781c04c4ab9045a165bacf428b043995a167f6.tar.gz) {};
    pkgs = import haskellNix.sources.nixpkgs haskellNix.nixpkgsArgs;
    iserv-proxy = pkgs.buildPackages.ghc-extra-packages.ghc8107.iserv-proxy.components.exes.iserv-proxy;
    remote-iserv = crossPkgs.ghc-extra-packages.ghc8107.remote-iserv.components.exes.remote-iserv;
    crossNumactl = crossPkgs.numactl;
    qemu = pkgs.buildPackages.qemu;
    qemuIservWrapper = pkgs.writeScriptBin "iserv-wrapper" ''
      #!${pkgs.stdenv.shell}
      set -euo pipefail
      # Unset configure flags as configure should have run already
      unset configureFlags
      # We try starting the remote-iserv process a few times,
      # in case the chosen port is taken.
      for((i=0;i<4;i++))
      do
          PORT=$((5000 + $RANDOM % 5000))
          (>&2 echo "---> Starting remote-iserv on port $PORT")
          rm -rf iserv-pipe
          mkfifo iserv-pipe
          exec 3<> iserv-pipe
          ${qemu}/bin/qemu-aarch64 ${remote-iserv}/bin/remote-iserv tmp $PORT -v &> iserv-pipe &
          RISERV_PID="$!"
          head -1 iserv-pipe | grep -q "Opening socket" && break
      done
      (>&2 echo "---| remote-iserv should have started on $PORT")
      ${iserv-proxy}/bin/iserv-proxy $@ 127.0.0.1 "$PORT"
      (>&2 echo "---> killing remote-iserve...")
      kill $RISERV_PID
      '';
    
    crossGHCLLVMWrapper = pkgs.writeScriptBin "ghc-llvm-wrapper" ''
      #!${pkgs.stdenv.shell}
      set -euo pipefail
      PATH="${pkgs.llvm_9}/bin:''${PATH:-}" ${crossGHC}/bin/aarch64-unknown-linux-gnu-ghc -pgmi ${qemuIservWrapper}/bin/iserv-wrapper -fexternal-interpreter -optl-L${crossNumactl}/lib "$@"
      '';
 
    crossGHC = crossPkgs.buildPackages.haskell-nix.compiler.ghc8107;
    crossGCC = crossPkgs.buildPackages.gcc;
    crossGCCUnwrapped = crossPkgs.buildPackages.gcc-unwrapped;
    crossBinutils = crossPkgs.buildPackages.binutils;
    crossBinutilsUnwrapped = crossPkgs.buildPackages.binutils-unwrapped;

    prefixStrippedGHC = pkgs.runCommand "ghc-aarch64-symlinks" {} ''
      mkdir -p $out/bin
      for tool in \
        ghc-8.10.7 \
        ghc-pkg \
        ghc-pkg-8.10.7 \
        ghci \
        ghci-8.10.7 \
        hp2ps \
        hpc \
        hsc2hs \
        runghc \
        runghc-8.10.7 \
        runhaskell
      do
          ln -s ${crossGHC}/bin/aarch64-unknown-linux-gnu-$tool $out/bin/$tool
      done;
      mkdir -p $out/lib
      ln -s ${crossGHC}/lib/aarch64-unknown-linux-gnu-ghc-8.10.7 $out/lib/ghc-8.10.7
      ln -s ${crossGHCLLVMWrapper}/bin/ghc-llvm-wrapper $out/bin/ghc
      touch $out/bin/haddock
      '';

    prefixStrippedGCC = pkgs.runCommand "gcc-aarch64-symlinks" {} ''
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
        crossGHCLLVMWrapper
      ];
  };
  cc-aarch64 = pkgs.buildEnv {
    name = "cc-aarch64-env";
    passthru = { isClang = false; };
    paths =
      [ prefixStrippedGCC
      ];
  };
  inherit pkgs;
}
