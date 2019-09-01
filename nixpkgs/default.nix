let
  sha256 = "1vpm73y7d0j2cviq0cgjwdj64h2v0c349capiyqf5f6071anx7d7";
  rev = "c4adeddb5f8";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
