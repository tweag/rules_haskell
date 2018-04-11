module Main (main) where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, execStateT, modify')
import Data.Maybe (fromJust)
import Data.Semigroup (Semigroup, sconcat)
import Distribution.PackageDescription.Parse
    (readGenericPackageDescription, parseHookedBuildInfo, ParseResult(..))
import Distribution.Text (display, simpleParse)
import Distribution.Verbosity (normal)
import System.Environment (getArgs)
import System.FilePath ((</>), (<.>), makeRelative, takeDirectory)
import System.Process (callProcess)
import Text.PrettyPrint (text, Doc)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Distribution.ModuleName as P
import qualified Distribution.Package as P
import qualified Distribution.PackageDescription as P
import qualified System.Directory as Directory

import Skylark
import Flatten
import Build

main :: IO ()
main = do
    [ghcVersionStr, packagesFile, prebuiltDepsFile, cabalFile, outFile] <- getArgs
    -- TODO: pass this through Bazel instead?
    packages <- Map.fromList
                    . map (\p -> (P.pkgName p, P.pkgVersion p))
                    . map (fromJust . simpleParse)
                    . lines 
                    <$> readFile packagesFile
    prebuiltDeps <- Map.fromList
                    . map (\p -> (P.pkgName p, P.pkgVersion p))
                    . map (fromJust . simpleParse)
                    . lines
                    <$> readFile prebuiltDepsFile
    gdesc <- readGenericPackageDescription normal cabalFile
    let ghcVersion = case simpleParse ghcVersionStr of
                      Nothing -> error $ "Error parsing ghc version: " ++ show ghcVersionStr
                      Just v -> v
    desc <- maybeConfigure $ flattenToDefaultFlags ghcVersion gdesc
    files <- getDirectoryContentsRecursive $ takeDirectory cabalFile
    writeFile outFile $ show $ renderStatements $
        buildRules files packages prebuiltDeps desc

maybeConfigure :: P.PackageDescription -> IO P.PackageDescription
maybeConfigure desc = case P.buildType desc of
    Just P.Configure -> do
        callProcess "./configure" []
        let buildInfoFile = display (P.packageName desc) <.> "buildinfo"
        buildInfoExists <- Directory.doesFileExist buildInfoFile
        if buildInfoExists
          then do
              hookedBIParse <- parseHookedBuildInfo <$> readFile buildInfoFile
              case hookedBIParse of
                  ParseFailed e -> error $ "Error reading buildinfo " ++ show buildInfoFile
                                              ++ ": " ++ show e
                  ParseOk _ hookedBI -> return $ P.updatePackageDescription hookedBI desc
          else return desc
    _ -> return desc

-- | Get the set of files in this distribution.
getDirectoryContentsRecursive :: FilePath -> IO PackageFiles
getDirectoryContentsRecursive f0 = fmap (Set.map $ makeRelative f0)
                                      $ execStateT (loop f0) Set.empty
  where
    loop f = do
        isDir <- lift $ Directory.doesDirectoryExist f
        if isDir
          then lift (Directory.getDirectoryContents f)
                  >>= mapM_ (loop . (f </>))
                        . filter regularFile
          else modify' (Set.insert f)
    regularFile "." = False
    regularFile ".." = False
    regularFile _ = True
