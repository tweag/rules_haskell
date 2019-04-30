-- | A program for reifying a .cabal file into a Bazel structure.
--
-- Takes as input a Cabal file and a GHC version.
-- Evaluates all of the flags in the file to generate a PackageDescription,
-- and then writes out a `.bzl` file containing a single definition:
--
--    package = struct(...)
--
-- which can be loaded into Bazel BUILD files.
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where

#if MIN_VERSION_Cabal(2,2,0)
import Distribution.PackageDescription.Parsec
    (readGenericPackageDescription)
#else
import Distribution.PackageDescription.Parse
    (readGenericPackageDescription)
#endif

import Distribution.Text (simpleParse)
import Distribution.Verbosity (normal)
import System.Environment (getArgs)

import qualified Data.Map.Strict as Map
import qualified Distribution.PackageDescription as P

import Description
import Flatten
import Skylark

main :: IO ()
main = do
    ghcVersionStr:cabalFile:outFile:flagArgs <- getArgs
    gdesc <- readGenericPackageDescription normal cabalFile
    let ghcVersion = case simpleParse ghcVersionStr of
                      Nothing -> error $ "Error parsing ghc version: " ++ show ghcVersionStr
                      Just v -> v
        packageFlags = parseFlags flagArgs
        desc = flattenToDefaultFlags ghcVersion packageFlags gdesc
    writeFile outFile $ show $ renderStatements
        [Assign "package" $ packageDescriptionExpr desc]

parseFlags :: [String] -> Map.Map P.FlagName Bool
parseFlags = \case
  "-flag-on":flag:etc -> Map.insert (P.mkFlagName flag) True (parseFlags etc)
  "-flag-off":flag:etc -> Map.insert (P.mkFlagName flag) False (parseFlags etc)
  _ -> Map.empty
