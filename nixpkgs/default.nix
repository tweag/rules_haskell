{ ... }@args:
let
  # 2023-10-20
  sha256 = "07dv5x236rqf5b718b7bhpqf2yaq43h6w8mrz9fcb0hxwjzsxlpg";
  rev = "80c1aab725151632ddc2a20caeb914e76dd0673c";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
}) args
