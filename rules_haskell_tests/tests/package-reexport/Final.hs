module Main where

import qualified TransitiveDep
import qualified Dep
import           Control.Exception (assert)

main :: IO ()
main = assert (TransitiveDep.whoAmI == Dep.whoIsMyDep) (pure ())
