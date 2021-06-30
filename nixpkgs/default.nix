{ ... }@args:
let
  # 2021-02-19
  sha256 = "1fwl898f6wznkjpwq11brgadz6iff5w5f4lwj2l7ax2rz7r03mnn";
  rev = "ad4db3f4d8ae54482c63c31c14921cb73953548d";

  # Recent GHC require otool and install_name_tool in PATH on MacOS.
  # These tools are only included in GHC's PATH since
  # https://github.com/nixos/nixpkgs/commit/118b28a127c79dbae8c513742fbb17f300407a4b.
  # Remove this override once the nixpkgs revision above is updated to a commit
  # after 118b28a127c79dbae8c513742fbb17f300407a4b.
  haskellSha256 = "10b4pnvf9yl7f2pabg402r49vhn1i6pkzdl7pp41z9v2pxnclxrm";
  haskellRev = "118b28a127c79dbae8c513742fbb17f300407a4b";
  haskellPkgs = import (fetchTarball {
    sha256 = haskellSha256;
    url = "https://github.com/NixOS/nixpkgs/archive/${haskellRev}.tar.gz";
  }) args;
  haskellOverlay = _: _: {
    inherit (haskellPkgs) haskell haskellPackages pandoc;
  };
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
}) (args // {
  overlays = [haskellOverlay];
})
