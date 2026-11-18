module LibWithData (readDataFile) where

import Paths_lib_with_data (getDataFileName)

readDataFile :: IO String
readDataFile = getDataFileName "datafile" >>= readFile
