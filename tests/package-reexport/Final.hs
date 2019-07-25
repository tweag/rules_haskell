module Main where

import qualified TransitiveDep
import qualified Dep
import           Control.Exception (assert)

main :: IO ()
main = pure $ assert (TransitiveDep.whoAmI == Dep.whoIsMyDep) ()
