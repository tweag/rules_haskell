{-# LANGUAGE LambdaCase #-}

module GHC.Paths (ghc, ghc_pkg, libdir, docdir) where

import qualified Bazel.Runfiles as Runfiles
import Control.Concurrent.MVar
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import System.FilePath (isAbsolute)
import System.IO.Unsafe (unsafePerformIO)

ghc, ghc_pkg, libdir, docdir :: FilePath
ghc = locatePath ghcMVar "RULES_HASKELL_GHC_PATH" "%GHC%"
ghc_pkg = locatePath ghc_pkgMVar "RULES_HASKELL_GHC_PKG_PATH" "%GHC_PKG%"
libdir = locatePath libdirMVar "RULES_HASKELL_LIBDIR_PATH" "%LIBDIR%"
docdir = locatePath docdirMVar "RULES_HASKELL_DOCDIR_PATH" "%DOCDIR%"

ghcMVar, ghc_pkgMVar, libdirMVar, docdirMVar :: MVar FilePath
ghcMVar = unsafePerformIO newEmptyMVar
ghc_pkgMVar = unsafePerformIO newEmptyMVar
libdirMVar = unsafePerformIO newEmptyMVar
docdirMVar = unsafePerformIO newEmptyMVar
{-# NOINLINE ghcMVar #-}
{-# NOINLINE ghc_pkgMVar #-}
{-# NOINLINE libdirMVar #-}
{-# NOINLINE docdirMVar #-}

locatePath :: MVar FilePath -> String -> FilePath -> FilePath
locatePath memo envVar runfilesPath =
  unsafePerformIO $
    tryReadMVar memo >>= \case
      Just path -> pure path
      Nothing -> do
        path <-
          if isAbsolute runfilesPath
            then pure runfilesPath
            else locatePathIO envVar runfilesPath
        _ <- tryPutMVar memo path
        pure path

locatePathIO :: String -> FilePath -> IO FilePath
locatePathIO envVar runfilesPath =
  lookupEnv envVar >>= \case
    Just path -> pure path
    Nothing -> do
      r <- Runfiles.create
      pure $! Runfiles.rlocation r runfilesPath

-- vim: ft=haskell
