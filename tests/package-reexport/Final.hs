module Main where

import qualified TransitiveDep 

whoIsMySubDep :: String
whoIsMySubDep = TransitiveDep.whoAmI

main :: IO ()
main = putStrLn whoIsMySubDep
