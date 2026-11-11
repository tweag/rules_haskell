# Official Go binary distribution, wrapped for `nixpkgs_go_configure`.
#
# The pinned nixpkgs (nixos-24.05) only goes up to Go 1.22, while gazelle and
# rules_go require Go >= 1.24.12. The official release tarball already has the
# layout rules_nixpkgs_go expects (the full SDK at the root), we only add the
# `ROOT` marker file.
let
  pkgs = import <nixpkgs> {
    config = { };
    overlays = [ ];
  };
  version = "1.25.0";
  # Checksums from https://go.dev/dl/?mode=json&include=all
  dists = {
    x86_64-linux = {
      platform = "linux-amd64";
      sha256 = "2852af0cb20a13139b3448992e69b868e50ed0f8a1e5940ee1de9e19a123b613";
    };
    aarch64-linux = {
      platform = "linux-arm64";
      sha256 = "05de75d6994a2783699815ee553bd5a9327d8b79991de36e38b66862782f54ae";
    };
    x86_64-darwin = {
      platform = "darwin-amd64";
      sha256 = "5bd60e823037062c2307c71e8111809865116714d6f6b410597cf5075dfd80ef";
    };
    aarch64-darwin = {
      platform = "darwin-arm64";
      sha256 = "544932844156d8172f7a28f77f2ac9c15a23046698b6243f633b0a0b00c0749c";
    };
  };
  system = pkgs.stdenv.hostPlatform.system;
  dist = dists.${system} or (throw "nixpkgs/go.nix: no Go ${version} binary distribution for ${system}");
in
pkgs.stdenvNoCC.mkDerivation {
  pname = "go-bin";
  inherit version;

  src = pkgs.fetchurl {
    url = "https://go.dev/dl/go${version}.${dist.platform}.tar.gz";
    inherit (dist) sha256;
  };

  dontConfigure = true;
  dontBuild = true;
  # The Go binaries are self-contained, leave them untouched.
  dontFixup = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out
    cp -r . $out/
    touch $out/ROOT
    runHook postInstall
  '';
}
