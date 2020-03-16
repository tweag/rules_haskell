let
  # 2020-01-29
  sha256 = "1iy3iz91xx58k9wgswalipa8gxxdafqq82lisg6s16ml8y232f00";
  rev = "42a195919a10ce70309c3666dfe8206e380fae1b";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
