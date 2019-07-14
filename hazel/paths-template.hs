{-# LANGUAGE ImplicitPrelude #-}
module %{module} (
    version,
    getDataDir,
    getDataFileName,
    ) where

import qualified Bazel.Runfiles as Runfiles
import Data.Version (Version, makeVersion)
import Prelude
import System.FilePath ((</>), takeDirectory)
import System.Environment (getExecutablePath)

-- TODO: automatically locate root directory
getDataDir :: IO FilePath
getDataDir = do
    r <- Runfiles.create
    pure $! Runfiles.rlocation r "%{data_dir}"

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
    dir <- getDataDir
    return (dir </> name)

-- TODO:
version :: Version
version = makeVersion %{version}
