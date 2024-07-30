{-# OPTIONS -Wall #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Exception.Safe (bracket, bracket_)
import Data.Foldable (for_)
import Data.List (delete, intercalate, isInfixOf, isPrefixOf, isSuffixOf, sort, stripPrefix)
import GHC.Stack (HasCallStack)
import System.Directory (copyFile, doesFileExist)
import System.FilePath ((</>))
import System.Info (os)
import System.IO (IOMode (..), hClose, hGetContents', openFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))

import qualified System.Process as Process
import Test.Hspec.Core.Spec (SpecM, SpecWith)
import Test.Hspec (context, hspec, it, describe, runIO, around_, afterAll_, expectationFailure, shouldBe)

import BinModule (b)
import GenModule (a)

import IntegrationTesting

main :: IO ()
main = hspec $  around_ printStatsHook $ do

  describe "rules_haskell_tests" $ afterAll_ (shutdownBazel ".") $  do
    it "bazel test" $ do
      assertSuccess (bazel ["test", "//..."])

    it "bazel test prof" $ do
      ghcVersion <- lookupEnv "GHC_VERSION"

      -- In .github/workflows/workflow.yaml we specify --test_tag_filters
      -- -dont_test_on_darwin. However, specifiying --test_tag_filters
      -- -requires_dynamic here alone would override that filter. So,
      -- we have to duplicate that filter here.
      let tagFilter | os == "darwin" = "-dont_test_on_darwin,-requires_dynamic,-skip_profiling" ++ (
                        -- skip tests for specific GHC version, see https://github.com/tweag/rules_haskell/issues/2073
                        maybe "" (",-dont_build_on_macos_with_ghc_" ++) ghcVersion)
                    | otherwise      = "-requires_dynamic,-skip_profiling"
      assertSuccess (bazel ["test", "-c", "dbg", "//...", "--build_tag_filters", tagFilter, "--test_tag_filters", tagFilter])

    it "bazel build worker" $ do
      assertSuccess (bazel ["build", "@rules_haskell//tools/worker:bin"])

    describe "stack_snapshot pinning" $
      it "handles packages in subdirectories correctly" $ do
        -- NOTE Keep in sync with
        --   .github/workflows/workflow.yaml
        let withBackup filename k =
              withSystemTempDirectory "bazel_backup" $ \tmp_dir -> do
                bracket_
                  (copyFile filename (tmp_dir </> "backup"))
                  (copyFile (tmp_dir </> "backup") filename)
                  k
        -- Test that pinning works and produces buildable targets.
        -- Backup the lock file to avoid unintended changes when run locally.
        withBackup "stackage-pinning-test_snapshot.json" $ do
          assertSuccess (bazel ["run", "@stackage-pinning-test-unpinned//:pin"])
          assertSuccess (bazel ["build", "@stackage-pinning-test//:hspec"])

    describe "repl" $ do
      it "for libraries" $ do
        assertSuccess (bazel ["run", "//tests/repl-targets:hs-lib-bad@repl", "--", "-ignore-dot-ghci", "-e", "1 + 2"])

      it "for binaries" $ do
        assertSuccess (bazel ["run", "//tests/binary-indirect-cbits:binary-indirect-cbits@repl", "--", "-ignore-dot-ghci", "-e", ":main"])

        assertSuccess (bazel ["run", "//tests/repl-targets:hs-test-bad@repl", "--", "-ignore-dot-ghci", "-e", "1 + 2"])

      it "with rebindable syntax" $ do
        let p' (stdout, _stderr) = lines stdout == ["True"]
        outputSatisfy p' (bazel ["run", "//tests/repl-targets:rebindable-syntax@repl", "--", "-ignore-dot-ghci", "-e", "check"])

      it "sets classpath" $ do
        assertSuccess (bazel ["run", "//tests/java_classpath:java_classpath@repl", "--", "-ignore-dot-ghci", "-e", ":main"])

      -- Test `compiler_flags` from toolchain and rule for REPL
      it "compiler flags" $ do
        assertSuccess (bazel ["run", "//tests/repl-flags:compiler_flags@repl", "--", "-ignore-dot-ghci", "-e", ":main"])

      -- Test make variable expansion in `compiler_flags` and `repl_ghci_args`.
      describe "make variables" $ do
        it "compiler flags" $ do
          assertSuccess (bazel ["run", "//tests/repl-make-variables:test-compiler-flags@repl", "--", "-ignore-dot-ghci", "-e", ":main"])
        it "indirect repl flags" $ do
          assertSuccess (bazel ["run", "//tests/repl-make-variables:repl-indirect-flags", "--", "-ignore-dot-ghci", "-e", ":main"])
        it "direct repl flags" $ do
          assertSuccess (bazel ["run", "//tests/repl-make-variables:repl-direct-flags", "--", "-ignore-dot-ghci", "-e", ":main"])

      -- Test `repl_ghci_args` from toolchain and rule for REPL
      it "repl flags" $ do
        assertSuccess (bazel ["run", "//tests/repl-flags:repl_flags@repl", "--", "-ignore-dot-ghci", "-e", "foo"])

      it "fails on multiple definitions" $ do
        assertSuccess (bazel ["run", "//tests/repl-multiple-definition:repl", "--", "-ignore-dot-ghci", "-e", "final"])

    describe "multi_repl" $ do
      it "loads transitive library dependencies" $ do
        let p' (stdout, _stderr) = lines stdout == ["tests/multi_repl/bc/src-c/BC/C.hs"]
        outputSatisfy p' (bazel ["run", "//tests/multi_repl:c_only_repl", "--", "-ignore-dot-ghci", "-e", ":show targets"])
      it "loads transitive source dependencies" $ do
        let p' (stdout, _stderr) = sort (lines stdout) == ["tests/multi_repl/a/src/A/A.hs","tests/multi_repl/bc/src-b/BC/B.hs","tests/multi_repl/bc/src-c/BC/C.hs"]
        outputSatisfy p' (bazel ["run", "//tests/multi_repl:c_multi_repl", "--", "-ignore-dot-ghci", "-e", ":show targets"])
      it "loads core library dependencies" $ do
        let p' (stdout, _stderr) = sort (lines stdout) == ["tests/multi_repl/core_package_dep/Lib.hs"]
        outputSatisfy p' (bazel ["run", "//tests/multi_repl:core_package_dep", "--", "-ignore-dot-ghci", "-e", ":show targets"])
      it "doesn't allow to manually load modules" $ do
        assertFailure (bazel ["run", "//tests/multi_repl:c_multi_repl", "--", "-ignore-dot-ghci", "-e", ":load BC.C", "-e", "c"])
      it "produces correct unit files for a repl flagged multi" $ do
        let unitFiles = ["a_a_unit_file", "bc_b_unit_file", "bc_c_unit_file", "bc_d_unit_file"]
            makeExpected prefix =
              [ "-hide-all-packages" ]
              ++ concatMap (\unitFile ->
                [ "-unit"
                , prefix ++ unitFile
                ]
              ) unitFiles
            stripPrefix' pfx object =
              case stripPrefix pfx object of
                Nothing -> error "unexpected prefix strip failure"
                Just stripped -> stripped
            readLines :: FilePath -> IO [String]
            readLines filePath = bracket
                (openFile filePath ReadMode)
                (hClose)
                (\h -> lines <$> hGetContents' h)
            checkUnitFile :: FilePath -> [String] -> [FilePath] -> [FilePath] -> IO ()
            checkUnitFile unitFile startDeps startSources startIncludes = do
              unitArgs <- readLines unitFile
              go unitArgs startDeps startSources startIncludes
             where
              go [] [] [] [] = pure ()
              go [] deps sources includes = expectationFailure(
                  "At least one of expected (dependecies, sources, include directories) was not-found:"
                  ++ (if (null deps) then "" else " missing dependencies: " ++ show deps)
                  ++ (if (null sources) then "" else " missing sources: " ++ show sources)
                  ++ (if (null includes) then "" else " missing include directories: " ++ show includes)
                )
              go ("-package-id":pkgId:rest) deps sources includes =
                if "base" `isPrefixOf` pkgId
                  then go rest deps sources includes
                  else if pkgId `elem` deps
                    then go rest (delete pkgId deps) sources includes
                    else expectationFailure ("unexpected dependency: " ++ pkgId)
              go (x:rest) deps sources includes | "-i" `isPrefixOf` x =
                let includeDir = drop 2 x
                 in if includeDir `elem` includes
                      then go rest deps sources (delete includeDir includes)
                      else expectationFailure ("unexpected include dir: " ++ x ++ "\n" ++ "expected includes:\n" ++ (intercalate "\n" startIncludes))
              go (x:rest) deps sources includes | "tests/" `isPrefixOf` x =
                if x `elem` sources
                  then go rest deps (delete x sources) includes
                  else expectationFailure ("unexpected source file: " ++ x)
              go (_:rest) deps sources includes | otherwise = go rest deps sources includes
            checkUnitFiles :: Process.CreateProcess -> IO ()
            checkUnitFiles cmd = do
              (exitCode, stdout, stderrCapture) <- Process.readCreateProcessWithExitCode cmd ""
              case exitCode of
                ExitFailure _ -> expectationFailure (formatOutput exitCode stdout stderrCapture)
                ExitSuccess -> do
                  let stdoutLines = lines stdout
                  case take 1 $ filter ("a_a_unit_file" `isSuffixOf`) stdoutLines of
                    [] -> expectationFailure "Could not find prefix to read unit files."
                    (_:_:_) -> error "take 1 returned a list of length 2 or greater"
                    [fullAUnitFile] -> do
                      let atPrefix = stripSuffix' "a_a_unit_file" fullAUnitFile
                          prefix = stripPrefix' "@" atPrefix
                          shortPrefix = stripSuffix' "_tests/multi_repl/" prefix
                          stripSuffix' sfx target = reverse $ stripPrefix' (reverse sfx) $ reverse target 
                          expandPath f = prefix ++ f
                      lines stdout `shouldBe` makeExpected atPrefix
                      checkUnitFile
                        (expandPath "a_a_unit_file")
                        []
                        ["tests/multi_repl/a/src/A/A.hs"]
                        ["tests/multi_repl/a/src", shortPrefix ++ "a/src"]
                      checkUnitFile
                        (expandPath "bc_b_unit_file")
                        ["testsZSmultiZUreplZSaZSa"]
                        ["tests/multi_repl/bc/src-b/BC/B.hs"]
                        ["tests/multi_repl/bc/src-b", shortPrefix ++ "bc/src-b"]
                      checkUnitFile
                        (expandPath "bc_c_unit_file")
                        ["testsZSmultiZUreplZSbcZSb"]
                        ["tests/multi_repl/bc/src-c/BC/C.hs"]
                        ["tests/multi_repl/bc/src-c", shortPrefix ++ "bc/src-c"]
                      checkUnitFile
                        (expandPath "bc_d_unit_file")
                        ["testsZSmultiZUreplZSbcZSc"]
                        ["tests/multi_repl/bc/src-d/BC/D.hs"]
                        ["tests/multi_repl/bc/src-d", shortPrefix ++ "bc/src-d"]
        checkUnitFiles (bazel ["run", "//tests/multi_repl:d_unit_repl@bios"])

    describe "ghcide" $ do
      it "loads RunTests.hs" $
        assertSuccess (Process.proc "./.ghcide" ["tests/RunTests.hs"])
      it "loads module with module dependency" $
        assertSuccess (Process.proc "./.ghcide" ["tests/binary-with-lib/Main.hs"])

    describe "transitive re-exports" $ do
      it "work" $
        assertSuccess (bazel ["build", "//tests/package-reexport-transitive"])
      it "work for long chains" $
        assertSuccess (bazel ["build", "//tests/package-reexport-transitive:long"])
      it "do not work for interrupted chains" $
        assertFailure (bazel ["build", "//tests/package-reexport-transitive:interrupted"])

    describe "failures" $ do
      -- Make sure not to include haskell_repl (@repl) or alias (-repl) targets
      -- in the query. Those would not fail under bazel test.
      all_failure_tests <- bazelQuery "kind('haskell_library|haskell_binary|haskell_test', //tests/failures/...) intersect attr('tags', 'manual', //tests/failures/...)"

      for_ all_failure_tests $ \test -> do
        it test $ do
          assertFailure (bazel ["build", test])

      context "known issues" $ do
        it "haskell_doc fails with plugins #1549" $
          -- https://github.com/tweag/rules_haskell/issues/1549
          assertFailure (bazel ["build", "//tests/haddock-with-plugin"])
        it "doctest failure with foreign import #1559" $
          -- https://github.com/tweag/rules_haskell/issues/1559
          assertFailure (bazel ["build", "//tests/haskell_doctest_ffi_1559:doctest-a"])

    -- Test that the repl still works if we shadow some Prelude functions
    it "repl name shadowing" $ do
      let p (stdout, stderrCapture) = not $ any ("error" `isInfixOf`) [stdout, stderrCapture]
      outputSatisfy p (bazel ["run", "//tests/repl-name-conflicts:lib@repl", "--", "-ignore-dot-ghci", "-e", "stdin"])

    -- GH2096: This test is flaky in CI using the MacOS GitHub runners. The flakiness is slowing 
    -- development on other features. Disable this test until a satisfying solution is found.
    -- it "Repl works with remote_download_toplevel" $ do
    --   let p (stdout, stderr) = not $ any ("error" `isInfixOf`) [stdout, stderr]
    --   withSystemTempDirectory "bazel_disk_cache" $ \tmp_disk_cache -> do
    --     assertSuccess $ bazel ["run", "//tests/multi_repl:c_only_repl", "--disk_cache=" <> tmp_disk_cache]
    --     assertSuccess $ bazel ["clean"]
    --     outputSatisfy p
    --       (bazel ["run", "//tests/multi_repl:c_only_repl", "--disk_cache=" <> tmp_disk_cache, "--remote_download_toplevel"])

  buildAndTest "../examples"
  buildAndTest "../tutorial"

-- * Bazel commands

-- | Returns a bazel command line suitable for CI
-- This should be called with the action as first item of the list. e.g 'bazel ["build", "//..."]'.
bazel :: [String] -> Process.CreateProcess
bazel args = Process.proc "bazel" args

-- | Runs a bazel query and return the list of matching targets
bazelQuery :: String -> SpecM a [String]
bazelQuery q = lines <$> runIO (Process.readProcess "bazel" ["query", q] "")

-- | Shutdown Bazel
shutdownBazel :: String -> IO ()
shutdownBazel path = do
  -- Related to https://github.com/tweag/rules_haskell/issues/2089
  -- We experience intermittent "Exit Code: ExitFailure (-9)" errors. Shutdown 
  -- Bazel when done executing tests for the workspace.
  assertSuccess (bazel ["shutdown"]) { Process.cwd = Just path }
  pure ()

buildAndTest :: HasCallStack => String -> SpecWith ()
buildAndTest path = describe path $ afterAll_ (shutdownBazel path) $ do
  it "bazel build" $ do
    assertSuccess $ (bazel ["build", "//..."]) { Process.cwd = Just path }
  it "bazel test" $ do
    assertSuccess $ (bazel ["test", "//..."]) { Process.cwd = Just path }

-- * Print Memory Hooks

-- | Print memory information before and after each test
-- Only perform the hook if RHT_PRINT_MEMORY is "true".
printStatsHook :: IO () -> IO ()
printStatsHook action = do
  rhtPrintMem <- lookupEnv "RHT_PRINT_MEMORY"
  case rhtPrintMem of
    Just "true" -> bracket_
                     (printStats "=== BEFORE ===")
                     (printStats "=== AFTER ===")
                     action
    _ -> action

topPath :: String
topPath = "/usr/bin/top"

dfPath :: String
dfPath = "/bin/df"

-- | Print information about the computer state to debug intermittent failures
-- Related to https://github.com/tweag/rules_haskell/issues/2089
printStats :: String -> IO ()
printStats msg = do
  -- Do not attempt to run top, if it does not exist.
  topExists <- doesFileExist topPath
  dfExists <- doesFileExist dfPath
  if topExists || dfExists then putStrLn msg else pure()
  if topExists then _printMemory else pure()
  if dfExists then _printDiskInfo else pure()

-- | Print information about the current memory state to debug intermittent failures
-- Related to https://github.com/tweag/rules_haskell/issues/2089
_printMemory :: IO ()
_printMemory = do
  (exitCode, stdOut, stdErr) <- Process.readProcessWithExitCode topPath ["-l", "1", "-s", "0", "-o", "mem", "-n", "15"] ""
  case exitCode of
    ExitSuccess -> putStrLn stdOut
    ExitFailure _ -> putStrLn ("=== _printMemory failed ===\n" ++ stdErr)

-- | Print information about the disk drives to debug intermittent failures
-- Related to https://github.com/tweag/rules_haskell/issues/2089
_printDiskInfo :: IO ()
_printDiskInfo = do
  (exitCode, stdOut, stdErr) <- Process.readProcessWithExitCode dfPath ["-H"] ""
  case exitCode of
    ExitSuccess -> putStrLn stdOut
    ExitFailure _ -> putStrLn ("=== _printDiskInfo failed ===\n" ++ stdErr)

-- Generated dependencies for testing the ghcide support
_ghciIDE :: Int
_ghciIDE = a + b
