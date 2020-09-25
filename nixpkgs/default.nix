{...}:
let
  # 2020-07-09
  sha256 = "1vi3wbvlvpd4200swd3594vps1fsnd7775mgzm3nnfs1imzkg00i";
  rev = "1d8018068278a717771e9ec4054dff1ebd3252b0";
  overlay = self: super: let inherit (self) writeTextFile; in {
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        ghc883 = super.haskell.packages.ghc883.override {
          overrides = self: super: {
            ghcPatched = super.ghc.overrideAttrs (attrs: {
              patches = attrs.patches or [] ++ [
                (writeTextFile {
                  name = "pgmc-supports-no-pie.patch";
                  text = ''
                    diff --git a/compiler/main/DynFlags.hs b/compiler/main/DynFlags.hs
                    index a7ec70f876..1e8f89918a 100644
                    --- a/compiler/main/DynFlags.hs
                    +++ b/compiler/main/DynFlags.hs
                    @@ -3004,6 +3004,8 @@ dynamic_flags_deps = [
                                                                   -- Don't pass -no-pie with -pgmc
                                                                   -- (see Trac #15319)
                                                                   sGccSupportsNoPie = False})))
                    +  , make_ord_flag defFlag "pgmc-supports-no-pie"
                    +      $ noArg $ alterSettings $ \s -> s { sGccSupportsNoPie = True }
                       , make_ord_flag defFlag "pgms"
                           (hasArg (\f -> alterSettings (\s -> s { sPgm_s   = (f,[])})))
                       , make_ord_flag defFlag "pgma"
                  '';
                })
              ];
            });
          };
        };
      };
    };
  };
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
}) { config = {}; overlays = [overlay]; }
