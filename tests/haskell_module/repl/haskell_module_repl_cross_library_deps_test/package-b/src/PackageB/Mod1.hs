module PackageB.Mod1 (PackageB.Mod1.mod1num) where

import qualified PackageA.Mod2

mod1num :: Int
mod1num = PackageA.Mod2.mod2num * 7
