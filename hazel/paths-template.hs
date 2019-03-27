{-# LANGUAGE ImplicitPrelude #-}
module %{module} (
    version,
    getDataDir,
    getDataFileName,
    ) where

import Data.Version (Version, makeVersion)
import Prelude
import System.FilePath ((</>), takeDirectory)
import System.Environment (getExecutablePath)

-- TODO: automatically locate root directory
getDataDir :: IO FilePath
getDataDir = do
    exePath <- getExecutablePath
    return $ takeDirectory exePath </> "%{base_dir}"

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
    dir <- getDataDir
    return (dir </> name)

-- TODO:
version :: Version
version = makeVersion %{version}
