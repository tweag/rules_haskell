{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitPrelude #-}
module {MODULE_NAME} where

import Data.Version (Version(..))

version :: Version
version = Version {VERSION_NUMBER_LIST} []

getDataFileName :: FilePath -> IO FilePath
getDataFileName f = (\d -> d ++ "/" ++ f) <$> getDataDir

getDataDir :: IO FilePath
getDataDir = {DATADIR_IMPL} -- TODO

