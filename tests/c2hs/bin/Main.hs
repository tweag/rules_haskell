module Main where

import Foo.Foo
import Bar
import Baz
import Code

main :: IO ()
main = do
  print foo
  print bar
  print baz
  intfun 10 >>= print
  boolfun 10 >>= print
