module Main (main) where

import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as E

import Lib (message)

main = T.putStrLn $ E.decodeUtf8 message
