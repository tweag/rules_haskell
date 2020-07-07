{-# LANGUAGE TupleSections #-}

-- | This module enables finding data dependencies ("runfiles") of Haskell
-- binaries at runtime.
--
-- For more information, see: https://github.com/bazelbuild/bazel/issues/4460
module Bazel.Runfiles
    ( Runfiles
    , create
    , createFromProgramPath
    , rlocation
    , env
    ) where

import Bazel.Arg0 (getArg0)
import Control.Monad (guard)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import Data.Foldable (asum)
import Data.List (find, isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe)
import GHC.Stack
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute)
import System.Environment (lookupEnv)
import qualified System.FilePath
import System.FilePath ((</>), (<.>), addTrailingPathSeparator, takeFileName)
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
-- supports looking up directories. This function allows to lookup a directory
-- in a manifest file, by looking for the first entry with a matching prefix
-- and then stripping the superfluous suffix.
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
--
-- This uses `argv[0]` to determine the path to the current program in
-- accordance to the above specification. If `argv[0]` is a relative path and
-- you changed the working directory before invoking 'create' then this mode of
-- runfiles detection will fail.
--
-- Note, that 'System.Environment.withProgName' and
-- 'System.Environment.withArgs' permanently modify `argv[0]` and break this
-- mode of runfiles path detection, see
-- https://gitlab.haskell.org/ghc/ghc/-/issues/18418.
--
-- To avoid these issues you can set the runfiles environment variables using
-- 'env' before calling @withProgName@ or @withArgs@, or you can use
-- 'createFromProgramPath' to specify the program path directly.
create :: HasCallStack => IO Runfiles
create = createFromProgramPath =<< getArg0

-- | Locate the runfiles directory or manifest for the current binary.
--
-- Identical to 'create' except that it accepts the path to the current program
-- as an argument rather than reading it from `argv[0]`.
createFromProgramPath :: HasCallStack => FilePath -> IO Runfiles
createFromProgramPath exePath = do
    mbRunfiles <- runMaybeT $ asum
      [ do
        -- Bazel sets RUNFILES_MANIFEST_ONLY=1 if the manifest file should be
        -- used instead of the runfiles root.
        manifestOnly <- liftIO $ lookupEnv manifestOnlyEnv
        guard (manifestOnly /= Just "1")
        -- Locate runfiles directory relative to executable or by environment.
        let tryRunfiles dir = do
              exists <- liftIO $ doesDirectoryExist dir
              guard exists
              liftIO $ makeAbsolute dir
        runfilesRoot <- asum
          [ tryRunfiles $ exePath <.> "runfiles"
          , do
            guard (os == "mingw32")
            tryRunfiles $ exePath <.> "exe" <.> "runfiles"
          , do
            dir <- MaybeT $ lookupEnv runfilesDirEnv
            tryRunfiles dir
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
        let tryManifest file = do
              exists <- liftIO $ doesFileExist file
              guard exists
              liftIO $ makeAbsolute file
        manifestPath <- asum
          [ tryManifest $ exePath <.> "runfiles_manifest"
          , do
            guard (os == "mingw32")
            tryManifest $ exePath <.> "exe" <.> "runfiles_manifest"
          , do
            file <- MaybeT $ lookupEnv manifestFileEnv
            tryManifest file
          ]
        content <- liftIO $ readFile manifestPath
        let mapping = parseManifest content
        pure $! RunfilesManifest manifestPath mapping
      ]

    case mbRunfiles of
        Just runfiles -> pure runfiles
        -- TODO: According to the specification this should not fail, but
        -- > 3. assume the binary has no runfiles.
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
            then anyM loop =<< fmap (map (fp </>)) (listDirectory fp)
            else pure $! takeFileName fp /= "MANIFEST"

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
        (normalize key, normalize $ dropWhile (== ' ') value)
