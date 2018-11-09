module Main
  ( main
  ) where

import qualified Args
import Cat (runCat)


main :: IO ()
main = Args.parse >>= runCat
