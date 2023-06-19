{ ... }@args:
let
  # 2023-05-22
  sha256 = "1lbr19fbh16n5ikgh9lzpscn83kr3w6hwx6md4k1iyxd7vx7zz0j";
  rev = "02df300699f8e4c24b39eeb7e034403880715dc5";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
}) args
