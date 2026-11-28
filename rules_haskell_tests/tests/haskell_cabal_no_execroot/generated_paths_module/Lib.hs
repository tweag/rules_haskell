module Lib (dataFileName) where

import Paths_no_execroot_generated (getDataFileName)

dataFileName :: IO FilePath
dataFileName = getDataFileName "datafile"
