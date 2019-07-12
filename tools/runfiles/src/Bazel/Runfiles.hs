{-# LANGUAGE TupleSections #-}

-- | This module enables finding data dependencies ("runfiles") of Haskell
-- binaries at runtime.
--
-- For more information, see: https://github.com/bazelbuild/bazel/issues/4460
module Bazel.Runfiles
    ( Runfiles
    , create
    , rlocation
    , env
    ) where

import Control.Monad (guard)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import Data.Foldable (asum)
import Data.List (find, isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getExecutablePath, lookupEnv)
import qualified System.FilePath
import System.FilePath (FilePath, (</>), (<.>), addTrailingPathSeparator)
import System.Info (os)

-- | Reference to Bazel runfiles, runfiles root or manifest file.
data Runfiles
  = RunfilesRoot !FilePath
    -- ^ The runfiles root directory.
  | RunfilesManifest !FilePath ![(FilePath, FilePath)]
    -- ^ The runfiles manifest file and its content.
  deriving Show


-- | Construct a path to a data dependency within the given runfiles.
--
-- For example: @rlocation \"myworkspace\/mypackage\/myfile.txt\"@
rlocation :: Runfiles -> FilePath -> FilePath
rlocation (RunfilesRoot f) g = f </> normalize g
rlocation (RunfilesManifest _ m) g = fromMaybe g' $ asum [lookup g' m, lookupDir g' m]
  where
    g' = normalize g

-- | Lookup a directory in the manifest file.
--
-- Bazel's manifest file only lists files. However, the @RunfilesRoot@ method
-- supports looking up directories, and this is an important feature for Cabal
-- path modules in Hazel. This function allows to lookup a directory in a
-- manifest file, by looking for the first entry with a matching prefix and
-- then stripping the superfluous suffix.
lookupDir :: FilePath -> [(FilePath, FilePath)] -> Maybe FilePath
lookupDir p = fmap stripSuffix . find match
  where
    p' = normalize $ addTrailingPathSeparator p
    match (key, value) = p' `isPrefixOf` key && drop (length p') key `isSuffixOf` value
    stripSuffix (key, value) = take (length value - (length key - length p')) value

-- | Normalize a path according to the Bazel specification.
--
-- See https://docs.google.com/document/d/e/2PACX-1vSDIrFnFvEYhKsCMdGdD40wZRBX3m3aZ5HhVj4CtHPmiXKDCxioTUbYsDydjKtFDAzER5eg7OjJWs3V/pub
--
-- "[...] returns the absolute path of the file, which is normalized (and
-- lowercase on Windows) and uses "/" as directory separator on every platform
-- (including Windows)"
normalize :: FilePath -> FilePath
normalize | os == "mingw32" = normalizeWindows
          | otherwise       = System.FilePath.normalise

-- | Normalize a path on Windows according to the Bazel specification.
normalizeWindows :: FilePath -> FilePath
normalizeWindows = map (toLower . normalizeSlash) . System.FilePath.normalise
  where
    normalizeSlash '\\' = '/'
    normalizeSlash c    = c


-- | Set environmental variables for locating the given runfiles directory.
--
-- Note that Bazel will set these automatically when it runs tests
-- (@bazel test@).  However, it will not automatically set them
-- during "bazel run"; thus, non-test binaries should set the
-- environment manually for processes that they call.
env :: Runfiles -> [(String, String)]
env (RunfilesRoot f) = [(runfilesDirEnv, f)]
env (RunfilesManifest f _) = [(manifestFileEnv, f), (manifestOnlyEnv, "1")]

runfilesDirEnv :: String
runfilesDirEnv = "RUNFILES_DIR"

manifestFileEnv :: String
manifestFileEnv = "RUNFILES_MANIFEST_FILE"

manifestOnlyEnv :: String
manifestOnlyEnv = "RUNFILES_MANIFEST_ONLY"


-- | Locate the runfiles directory or manifest for the current binary.
--
-- This behaves according to the specification in:
-- https://docs.google.com/document/d/e/2PACX-1vSDIrFnFvEYhKsCMdGdD40wZRBX3m3aZ5HhVj4CtHPmiXKDCxioTUbYsDydjKtFDAzER5eg7OjJWs3V/pub
create :: IO Runfiles
create = do
    exePath <- getExecutablePath

    mbRunfiles <- runMaybeT $ asum
      [ do
        -- Bazel sets RUNFILES_MANIFEST_ONLY=1 if the manifest file should be
        -- used instead of the runfiles root.
        manifestOnly <- liftIO $ lookupEnv manifestOnlyEnv
        guard (manifestOnly /= Just "1")
        -- Locate runfiles directory relative to executable or by environment.
        runfilesRoot <- asum
          [ do
            let dir = exePath <.> "runfiles"
            exists <- liftIO $ doesDirectoryExist dir
            guard exists
            pure dir
          , do
            dir <- MaybeT $ lookupEnv runfilesDirEnv
            exists <- liftIO $ doesDirectoryExist dir
            guard exists
            pure dir
          ]
        -- Existence alone is not sufficient, on Windows Bazel creates a
        -- runfiles directory containing only MANIFEST. We need to check that
        -- more entries exist, before we commit to using the runfiles
        -- directory.
        containsData <- liftIO $ containsOneDataFile runfilesRoot
        guard containsData
        pure $! RunfilesRoot runfilesRoot
      , do
        -- Locate manifest file relative to executable or by environment.
        manifestPath <- asum
          [ do
            let file = exePath <.> "runfiles_manifest"
            exists <- liftIO $ doesFileExist file
            guard exists
            pure file
          , do
            file <- MaybeT $ lookupEnv manifestFileEnv
            exists <- liftIO $ doesFileExist file
            guard exists
            pure file
          ]
        content <- liftIO $ readFile manifestPath
        let mapping = parseManifest content
        pure $! RunfilesManifest manifestPath mapping
      ]

    case mbRunfiles of
        Just runfiles -> pure runfiles
        Nothing -> error "Unable to locate runfiles directory or manifest"

-- | Check if the given directory contains at least one data file.
--
-- Traverses the given directory tree until a data file is found. A file named
-- @MANIFEST@ does not count, as it corresponds to @runfiles_manifest@.
--
-- Assumes that the given filepath exists and is a directory.
containsOneDataFile :: FilePath -> IO Bool
containsOneDataFile = loop
  where
    loop fp = do
        isDir <- doesDirectoryExist fp
        if isDir
            then anyM loop =<< listDirectory fp
            else pure $! fp /= "MANIFEST"

-- | Check if the given predicate holds on any of the given values.
--
-- Short-circuting:
-- Stops iteration on the first element for which the predicate holds.
anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM predicate = loop
  where
    loop [] = pure False
    loop (x:xs) = do
        b <- predicate x
        if b
            then pure True
            else loop xs

-- | Parse Bazel's manifest file content.
--
-- The manifest file holds lines of keys and values separted by a space.
parseManifest :: String -> [(FilePath, FilePath)]
parseManifest = map parseLine . lines
  where
    parseLine l =
        let (key, value) = span (/= ' ') l in
        (key, dropWhile (== ' ') value)
