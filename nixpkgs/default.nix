{ ... }@args:
let
  # 2021-06-24
  sha256 = "15w3yr8nash6ya67q9mcak3z7ba8vn39jq6ynpfxgagmxj7j0jyb";
  rev = "65db0350fe3962c41b8604046ec9166976f80793";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
}) args
