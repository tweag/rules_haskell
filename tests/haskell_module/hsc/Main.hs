module Main (main) where

import BinHsc ()
import Foo (hscFiredFoo)
import Bar (hscFiredBar)
import Bar.Baz (hscFiredBaz)
import Flags (hscFlags)

main :: IO ()
main = putStrLn (hscFiredFoo ++ hscFiredBar ++ hscFiredBaz ++ hscFlags)
