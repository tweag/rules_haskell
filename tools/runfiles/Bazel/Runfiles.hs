-- | This module enables finding data dependencies ("runfiles") of Haskell
-- binaries at runtime.
--
-- For more information, see: https://github.com/bazelbuild/bazel/issues/4460
--
-- Note: this does not currently support the RUNFILES_MANIFEST environmental
-- variable.  However, that's only necessary on Windows, which rules_haskell
-- doesn't support yet.
--
-- Additionally, this is not yet supported by the REPL.
module Bazel.Runfiles
    ( Runfiles
    , getRunfiles
    , rlocation
    , runfilesEnv
    ) where

import System.Directory (doesDirectoryExist)
import System.Environment (getExecutablePath, lookupEnv)
import System.FilePath (FilePath, (</>), (<.>))

-- | A path to a directory tree containing runfiles for the given
newtype Runfiles = Runfiles FilePath
    deriving Show

-- | Construct a path to a data dependency within the given runfiles.
--
-- For example: @rlocation \"myworkspace/mypackage/myfile.txt\"@
rlocation :: Runfiles -> FilePath -> FilePath
rlocation (Runfiles f) g = f </> g

-- | Set environmental variables for locating the given runfiles directory.
--
-- Note that Bazel will set these automatically when it runs tests
-- (@bazel test@).
runfilesEnv :: Runfiles -> [(String, String)]
runfilesEnv (Runfiles f) = [(runfilesDirEnv, f)]

runfilesDirEnv :: String
runfilesDirEnv = "RUNFILES_DIR"

-- | Locate the runfiles directory for the current binary.
--
-- This behaves according ot the specification in:
-- https://docs.google.com/document/d/e/2PACX-1vSDIrFnFvEYhKsCMdGdD40wZRBX3m3aZ5HhVj4CtHPmiXKDCxioTUbYsDydjKtFDAzER5eg7OjJWs3V/pub
--
-- Note: it does not currently support the @RUNFILES_MANIFEST@ environmental
-- variable.  However, that's only necessary on Windows, which rules_haskell
-- doesn't support yet anyway.
getRunfiles :: IO Runfiles
getRunfiles = do
    exeRunfilesPath <- fmap (<.> "runfiles") getExecutablePath
    exeRunfilesExists <- doesDirectoryExist exeRunfilesPath
    if exeRunfilesExists
      then return $ Runfiles exeRunfilesPath
      else do
        envDir <- lookupEnv runfilesDirEnv
        case envDir of
            Just f -> return $ Runfiles f
            Nothing -> error "Unable to locate runfiles directory"
