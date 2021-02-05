let
  # 2021-02-04
  sha256 = "1bkjh566r1bpbddz6fjhccn872p1dlvg5fwq9j2qdj4b5q2pmljc";
  rev = "24e5fe6075bc7a137bb701eb8a378f5a8689e10d";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
