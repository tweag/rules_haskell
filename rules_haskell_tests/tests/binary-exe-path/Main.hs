module Main where

import System.Environment

main = getExecutablePath >>= putStrLn
