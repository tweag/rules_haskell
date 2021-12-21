{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import GHC.Check
import qualified GHC.Paths

ghcVersionChecker :: GhcVersionChecker
ghcVersionChecker =
  $$(makeGhcVersionChecker (pure $ GHC.Paths.libdir))

main :: IO ()
main = do
  installationCheck <- ghcVersionChecker GHC.Paths.libdir
  case installationCheck of
    InstallationNotFound {..} ->
      fail $ "GHC installation not found in libdir: " <> libdir
    InstallationMismatch {..} ->
      fail $ "GHC version mismatch in libdir: " <> libdir
        <> ": got " <> show runTime
        <> " expected: " <> show compileTime
    InstallationChecked {} ->
      pure ()
