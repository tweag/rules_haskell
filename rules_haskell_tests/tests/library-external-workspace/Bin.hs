module Main (main) where

import TestLib (testMessage)
import qualified TestLib2 (testMessage)

main :: IO ()
main = putStrLn $ testMessage ++ TestLib2.testMessage
