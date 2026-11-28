module Lib (dataFileName) where

import Paths_no_execroot_cabal (getDataFileName)

dataFileName :: IO FilePath
dataFileName = getDataFileName "datafile"
