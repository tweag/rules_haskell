module Lib where

import Runfiles
import System.IO (IOMode (ReadMode), hGetLine, withFile)

content :: IO String
content = do
  r <- Runfiles.create
  let location = Runfiles.rlocation r "other_module/datafile"
  withFile location ReadMode hGetLine
