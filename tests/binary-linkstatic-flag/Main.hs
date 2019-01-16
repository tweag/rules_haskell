module Main (main) where

import qualified CLib
import qualified HsLib

main = print $ HsLib.value + CLib.value
