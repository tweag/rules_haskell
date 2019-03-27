module Main (main) where

import Control.Exception (assert)
import Data.Vector as V

main = assert (55 == V.sum (V.enumFromN 1 10)) $ return ()
