let
  sha256 = "093n9ci000vlhz700mrkdv7jnx6szgmggl0x0wbr1d0yrg2y7q43";
  rev = "f612ab79002c546d668c4f4ee3d5321fc814b838";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
