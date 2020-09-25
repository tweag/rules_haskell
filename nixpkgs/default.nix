{...}:
let
  # 2020-07-09
  sha256 = "1vi3wbvlvpd4200swd3594vps1fsnd7775mgzm3nnfs1imzkg00i";
  rev = "1d8018068278a717771e9ec4054dff1ebd3252b0";
  overlay = self: super: let inherit (self) writeTextFile; in {
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        ghc8101 = super.haskell.packages.ghc8101.override {
          overrides = self: super: {
            ghc = super.ghc.overrideAttrs (attrs: {
              patches = attrs.patches or [] ++ [
                (writeTextFile {
                  name = "pgmc-supports-no-pie.patch";
                  text = ''
                    diff --git a/compiler/main/DynFlags.hs b/compiler/main/DynFlags.hs
                    index 97a4c58ceb..32a87ce4da 100644
                    --- a/compiler/main/DynFlags.hs
                    +++ b/compiler/main/DynFlags.hs
                    @@ -3067,6 +3067,8 @@ dynamic_flags_deps = [
                                -- (see #15319)
                                toolSettings_ccSupportsNoPie = False
                              }
                    +  , make_ord_flag defFlag "pgmc-supports-no-pie"
                    +      $ noArg $ alterToolSettings $ \s -> s { toolSettings_ccSupportsNoPie = True }
                       , make_ord_flag defFlag "pgms"
                           (HasArg (\_ -> addWarn "Object splitting was removed in GHC 8.8"))
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
