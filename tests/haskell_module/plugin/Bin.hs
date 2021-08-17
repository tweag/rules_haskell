module Main (main) where

import ModuleWith (part1)
import ModuleWithout (part2)

main :: IO ()
main =
  if "plugin" == ['a', 'c', 't', 'i', 'v', 'e'] then
    error "plugin loaded unexpectedly in binary"
  else
    putStrLn $ part1 ++ part2
