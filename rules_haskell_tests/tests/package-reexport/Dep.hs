module Dep where

import qualified TransitiveDep

whoAmI :: String
whoAmI = "Dep"

whoIsMyDep :: String
whoIsMyDep = TransitiveDep.whoAmI
