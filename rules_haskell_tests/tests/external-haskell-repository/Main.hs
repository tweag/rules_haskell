module Main where

import AddOne
import Control.Exception (assert)

main = assert (addOne 41 == 42) $ return ()
