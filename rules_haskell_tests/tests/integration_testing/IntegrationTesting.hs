module IntegrationTesting
    ( bazelCmd
    , setupWorkspace
    , setupTestBazel
    , assertSuccess
    , assertFailure
    , outputSatisfy
    , failedOutputSatisfy
    , formatOutput
    ) where

import qualified System.Process as Process
import System.Environment (getEnv, unsetEnv, lookupEnv)
import System.Info (os)
import System.FilePath (pathSeparators, (</>))
import System.Directory (copyFile, doesDirectoryExist, doesFileExist, removePathForcibly, createDirectory, listDirectory, doesPathExist)
import Data.Text (pack, unpack, replace, breakOn)
import Data.Text.IO (readFile, writeFile)
import System.Exit (ExitCode(..))
import Control.Monad (when, unless, forM_)
import Test.Hspec (shouldSatisfy, expectationFailure)
import qualified Bazel.Runfiles as Runfiles

bazelCmd :: String -> String -> IO ([String] -> Process.CreateProcess)
bazelCmd workspaceDir outputUserRoot = do
    bazelPath <- getEnv "BIT_BAZEL_BINARY"
    config <- (fmap bazelConfig isNixpkgs)
    let bazelConfigurableSubcommands =
            ["aquery", "build", "canonicalize-flags", "clean", "coverage", "cquery", "info", "mobile-install", "print_action", "run", "test"]
    return (\args -> case args of
        subcommand:xs | elem subcommand bazelConfigurableSubcommands -> (Process.proc bazelPath (["--output_user_root", outputUserRoot, subcommand, "--config", config] ++ xs)) { Process.cwd = Just workspaceDir }
        xs -> (Process.proc bazelPath (["--output_user_root", outputUserRoot] ++ xs)) { Process.cwd = Just workspaceDir })

isNixpkgs :: IO Bool
isNixpkgs = lookupEnv "NIXPKGS" >>= \value ->
    case value of
        Just "1" -> pure True
        _ -> pure False

bazelConfig :: Bool -> String
bazelConfig isnix
    | isnix = case os of
        "darwin" -> "macos-nixpkgs"
        _ -> "linux-nixpkgs"
    | otherwise = case os of
        "darwin" -> "macos-bindist"
        "mingw32" -> "windows-bindist"
        _ -> "linux-bindist"

outputBaseDir :: IO String
outputBaseDir = do
    tmpDir <- getEnv "TEST_TMPDIR"
    return (unpack . fst $ breakOn (pack (pathSeparators ++ "execroot" ++ pathSeparators)) (pack tmpDir))

replaceInFile :: FilePath -> String -> String -> IO ()
replaceInFile path from to = do
    content <- Data.Text.IO.readFile path
    Data.Text.IO.writeFile path (replace (pack from) (pack to) content)

removeDirIfExist :: FilePath -> IO ()
removeDirIfExist path = do
    dirExist <- doesDirectoryExist path
    when dirExist (removePathForcibly path)

createDirIfNotExist :: FilePath -> IO ()
createDirIfNotExist path = do
    dirExist <- doesDirectoryExist path
    unless dirExist (createDirectory path)

generateBazelRc :: FilePath -> IO ()
generateBazelRc dir = do
    alreadyExist <- doesFileExist (dir </> ".bazelrc")
    unless alreadyExist $ Data.Text.IO.writeFile (dir </> ".bazelrc") (pack " \n\
\ build:linux-nixpkgs --host_platform=@io_tweag_rules_nixpkgs//nixpkgs/platforms:host \n\
\ build:linux-nixpkgs --incompatible_enable_cc_toolchain_resolution \n\
\ build:macos-nixpkgs --host_platform=@io_tweag_rules_nixpkgs//nixpkgs/platforms:host \n\
\ build:macos-nixpkgs --incompatible_enable_cc_toolchain_resolution \n\
\ build:linux-bindist --incompatible_enable_cc_toolchain_resolution \n\
\ build:macos-bindist --incompatible_enable_cc_toolchain_resolution \n\
\ build:windows-bindist --incompatible_enable_cc_toolchain_resolution \n\
\ ")

setupWorkspace :: IO (String, String)
setupWorkspace = do
    runfiles <- Runfiles.create
    workspaceDir <- getEnv "BIT_WORKSPACE_DIR"
    bazelBinId <- getEnv "BIT_BAZEL_BIN_ID"
    outputBase <- outputBaseDir
    let execDir = outputBase </> "bazel_testing"
    createDirIfNotExist execDir
    let newWorkspaceDir = execDir </> bazelBinId
    let outputUserRoot = outputBase
    removeDirIfExist newWorkspaceDir
    copyDirectoryRecursive workspaceDir newWorkspaceDir
    generateBazelRc newWorkspaceDir
    removeDirIfExist (execDir </> "rules_haskell")
    copyDirectoryRecursive (Runfiles.rlocation runfiles "rules_haskell") (execDir </> "rules_haskell")
    replaceInFile (newWorkspaceDir </> "WORKSPACE") "%RULES_HASKELL_PATH%" "../rules_haskell"
    return (newWorkspaceDir, outputUserRoot)

setupTestBazel :: IO ([String] -> Process.CreateProcess)
setupTestBazel = setupWorkspace >>= uncurry bazelCmd

-- * Action helpers

-- | Ensure that @(stdout, stderr)@ of the command satisfies a predicate
outputSatisfy
  :: ((String, String) -> Bool)
  -> Process.CreateProcess
  -> IO ()
outputSatisfy predicate cmd = do
  (exitCode, stdout, stderr) <- Process.readCreateProcessWithExitCode cmd ""

  case exitCode of
    ExitSuccess -> (stdout, stderr) `shouldSatisfy` predicate
    ExitFailure _ -> expectationFailure (formatOutput exitCode stdout stderr)

-- | Ensure that command is failed and @(stdout, stderr)@ of the command satisfies a predicate
failedOutputSatisfy
  :: ((String, String) -> Bool)
  -> Process.CreateProcess
  -> IO ()
failedOutputSatisfy predicate cmd = do
  (exitCode, stdout, stderr) <- Process.readCreateProcessWithExitCode cmd ""

  case exitCode of
    ExitFailure _ -> (stdout, stderr) `shouldSatisfy` predicate
    ExitSuccess -> expectationFailure (formatOutput exitCode stdout stderr)

-- | The command must succeed
assertSuccess :: Process.CreateProcess -> IO ()
assertSuccess = outputSatisfy (const True)

-- | The command must fail
assertFailure :: Process.CreateProcess -> IO ()
assertFailure cmd = do
  (exitCode, stdout, stderr) <- Process.readCreateProcessWithExitCode cmd ""

  case exitCode of
    ExitFailure _ -> pure ()
    ExitSuccess -> expectationFailure ("Unexpected success of a failure test with output:\n" ++ formatOutput exitCode stdout stderr)

-- * Formatting helpers

formatOutput :: ExitCode -> String -> String -> String
formatOutput exitcode stdout stderr =
  let
    header = replicate 20 '-'
    headerLarge = replicate 20 '='

  in unlines [
      headerLarge
    , "Exit Code: " <> show exitcode
    , headerLarge
    , "Standard Output"
    , header
    , stdout
    , headerLarge
    , "Error Output"
    , header
    , stderr
    , header]

copyDirectoryRecursive :: FilePath -> FilePath -> IO ()
copyDirectoryRecursive srcDir dstDir = do
    unlessM (doesPathExist dstDir) (createDirectory dstDir)
    entries <- listDirectory srcDir
    forM_ entries $ \name -> do
        let srcPath = srcDir </> name
        let dstPath = dstDir </> name
        isDir <- doesDirectoryExist srcPath
        if isDir
            then copyDirectoryRecursive srcPath dstPath
            else copyFile srcPath dstPath
    where
        unlessM b f = do b <- b; if b then pure() else f
