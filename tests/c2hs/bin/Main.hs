module Main where

import Foo.Foo
import Bar
import Baz

main :: IO ()
main = do
  print foo
  print bar
  print baz
