let
  # 2020-04-20
  sha256 = "011f36kr3c1ria7rag7px26bh73d1b0xpqadd149bysf4hg17rln";
  rev = "10100a97c8964e82b30f180fda41ade8e6f69e41";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
