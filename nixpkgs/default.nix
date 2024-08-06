{ ... }@args:
let
  # nixos-24.05 @ 2024-07-02
  sha256 = "sha256:0bpw6x46mp0xqfdwbrhnjn6qlb4avglir993n3cdqg8zv4klgllw";
  rev = "706eef542dec88cc0ed25b9075d3037564b2d164";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
}) args
