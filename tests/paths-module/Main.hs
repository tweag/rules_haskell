module Main (main) where

import Data.Version (showVersion)
import Paths_paths_module (version)

main :: IO ()
main = putStrLn (showVersion version)
