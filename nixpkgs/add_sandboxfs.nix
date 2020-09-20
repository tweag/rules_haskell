self: super:

let
  sandboxfs = super.rustPlatform.buildRustPackage rec {
    pname = "sandboxfs";
    version = "0.2.0";

    src = super.fetchFromGitHub {
      owner = "bazelbuild";
      repo = pname;
      rev = "sandboxfs-${version}";
      sha256 = "1xgz7nxhn5x5yg1ak12kj9mjpvs3g049bqxmpkksdqadl6mspbi1";
    };

    cargoPatches = [ ./add_cargo_lock.patch ];

    nativeBuildInputs = [ self.pkgconfig ];
    buildInputs = [ self.fuse ];

    cargoSha256 = "1pz5b57x3n5s7wjrn4d0knb7jyb1wgplckjn7kp7j7s5pcx15bp9";
    verifyCargoDeps = false;

    meta = with super.stdenv.lib; {
      description = "A virtual file system for sandboxing";
      homepage = https://github.com/bazelbuild/sandboxfs;
      license = licenses.asl20;
      maintainers = [];
      platforms = platforms.all;
    };
  };
in
{
  inherit sandboxfs;
}
