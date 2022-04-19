module Main (main) where

import BinHsc ()
import Foo (hscFiredFoo)
import Bar (hscFiredBar)
import Bar.Baz (hscFiredBaz)
import Flags (hscFlags)
import qualified WithHeader.Point
import qualified WithHeader.PointRelativePath
import Control.Monad (unless)
import Foreign.Storable (poke, peek)
import Foreign.Marshal.Alloc (malloc)

main :: IO ()
main = do
  putStrLn (hscFiredFoo ++ hscFiredBar ++ hscFiredBaz ++ hscFlags)

  do
    ptr <- malloc
    poke ptr (WithHeader.Point.MkPoint 42 1337)
    (WithHeader.Point.MkPoint x y) <- peek ptr
    unless (x == 42 && y == 1337) $ fail "didn't peek what we poked for WithHeader.Point"

  do
    ptr <- malloc
    poke ptr (WithHeader.PointRelativePath.MkPoint 42 1337)
    (WithHeader.PointRelativePath.MkPoint x y) <- peek ptr
    unless (x == 42 && y == 1337) $ fail "didn't peek what we poked for WithHeader.PointRelativePath"
