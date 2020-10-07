{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wwarn #-}

import Data.List (isPrefixOf)
import qualified Data.Map as Map
import GHC.Base (String)
import GHC.Err (error)
import qualified Options.Applicative as Opt
import Data.Traversable (for)
import Path (Dir, File, Path)
import qualified Path
import Path.IO (resolveFile')
import System.IO (FilePath, IO)
import Trace.Hpc.Lcov (generateLcovFromTix, writeReport)
import Trace.Hpc.Mix (Mix (..), readMix)
import Trace.Hpc.Tix (Tix (..), TixModule, readTix, tixModuleName)

data CLIOptions = CLIOptions
  { cliTixFiles :: [FilePath],
    cliMainPackage :: Maybe String,
    cliOutput :: FilePath,
    mixDirectories :: [FilePath],
    packageDirectories :: [String],
    sourceRoot :: FilePath
  }

getCLIOptions :: IO CLIOptions
getCLIOptions =
  Opt.execParser $
    Opt.info (Opt.helper <*> parseCLIOptions) $ Opt.progDesc description
  where
    parseCLIOptions =
      CLIOptions
        <$> parseCLITixFiles
        <*> parseCLIMainPackage
        <*> parseCLIOutput
        <*> parseMixDirectories
        <*> parsePackageDirectories
        <*> parseSourceRoot
    parseCLITixFiles =
      Opt.many $
        Opt.strArgument $
          mconcat
            [ Opt.metavar "FILE",
              Opt.help "Manually specify .tix file(s) to convert"
            ]
    parseSourceRoot =
      Opt.strOption $
        mconcat
          [ Opt.metavar "FILE",
            Opt.short 'r',
            Opt.long "root",
            Opt.help "Specify the root of the sources files"
          ]
    parseMixDirectories =
      Opt.some $
        Opt.strOption $
          mconcat
            [ Opt.long "mix",
              Opt.short 'm',
              Opt.metavar "FILE",
              Opt.help "Manually specify .mix file(s) directories"
            ]
    parsePackageDirectories =
      Opt.many $
        Opt.strOption $
          mconcat
            [ Opt.long "package",
              Opt.short 'p',
              Opt.metavar "PACKAGE_NAME=PACKAGE_PATH",
              Opt.help "Manually specify package file(s) directories"
            ]
    parseCLIMainPackage =
      Opt.optional $
        Opt.strOption $
          mconcat
            [ Opt.long "main-package",
              Opt.metavar "PACKAGE",
              Opt.help "The package that built the coverage-enabled executable"
            ]
    parseCLIOutput =
      Opt.strOption $
        mconcat
          [ Opt.long "output",
            Opt.short 'o',
            Opt.metavar "FILE",
            Opt.help "The file to save coverage information (default: lcov.info)",
            Opt.value "lcov.info"
          ]

    description = "Convert HPC coverage output into the LCOV format"

main :: IO ()
main = do
  CLIOptions {..} <- getCLIOptions

  tixFiles <- traverse resolveFile' cliTixFiles

  tixModules <- filter (not . isPathsModule) . concat <$> traverse readTixPath tixFiles

  -- TODO: see how this can be pushed in the opt parser
  mixDirectories' <- traverse Path.parseAbsDir mixDirectories

  moduleToMixList <- for tixModules $ \tixModule -> do
    Mix fileLoc _ _ _ mixEntries <- readMixPath mixDirectories' (Right tixModule)
    fileLocRelPath <- Path.parseRelFile fileLoc

    let modulePath = fileLocRelPath

    pure (tixModuleName tixModule, (Path.toFilePath modulePath, mixEntries))

  let moduleToMix = Map.toList . Map.fromListWith checkDupeMix $ moduleToMixList
      checkDupeMix mix1 mix2 =
        if mix1 == mix2
          then mix1
          else error $ ".mix files differ: " <> show (mix1, mix2)
      report = generateLcovFromTix moduleToMix tixModules

  writeReport cliOutput report

{- HPC file readers -}

readTixPath :: Path b File -> IO [TixModule]
readTixPath path = do
  Tix tixModules <-
    readTix (Path.toFilePath path)
      >>= maybe (error $ "Could not find tix file: " <> Path.toFilePath path) pure
  pure tixModules

readMixPath :: [Path b Dir] -> Either String TixModule -> IO Mix
readMixPath = readMix . fmap Path.toFilePath

{- Haskell package/module helpers -}

isPathsModule :: TixModule -> Bool
isPathsModule = ("Paths_" `isPrefixOf`) . tixModuleName
