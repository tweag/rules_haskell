{-# OPTIONS -Wall #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Exception.Safe (bracket_)
import Data.Foldable (for_)
import Data.List (isInfixOf, sort)
import System.Directory (copyFile)
import System.FilePath ((</>))
import System.Info (os)
import System.IO.Temp (withSystemTempDirectory)
import System.Environment (lookupEnv)

import qualified System.Process as Process
import Test.Hspec.Core.Spec (SpecM)
import Test.Hspec (context, hspec, it, describe, runIO, afterAll_)

import BinModule (b)
import GenModule (a)

import IntegrationTesting

main :: IO ()
main = hspec $ do
  afterAll_ shutdownBazel $ do
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
        let p' (stdout, _stderr) = lines stdout == ["tests/multi_repl/bc/src/BC/C.hs"]
        outputSatisfy p' (bazel ["run", "//tests/multi_repl:c_only_repl", "--", "-ignore-dot-ghci", "-e", ":show targets"])
      it "loads transitive source dependencies" $ do
        let p' (stdout, _stderr) = sort (lines stdout) == ["tests/multi_repl/a/src/A/A.hs","tests/multi_repl/bc/src/BC/B.hs","tests/multi_repl/bc/src/BC/C.hs"]
        outputSatisfy p' (bazel ["run", "//tests/multi_repl:c_multi_repl", "--", "-ignore-dot-ghci", "-e", ":show targets"])
      it "loads core library dependencies" $ do
        let p' (stdout, _stderr) = sort (lines stdout) == ["tests/multi_repl/core_package_dep/Lib.hs"]
        outputSatisfy p' (bazel ["run", "//tests/multi_repl:core_package_dep", "--", "-ignore-dot-ghci", "-e", ":show targets"])
      it "doesn't allow to manually load modules" $ do
        assertFailure (bazel ["run", "//tests/multi_repl:c_multi_repl", "--", "-ignore-dot-ghci", "-e", ":load BC.C", "-e", "c"])

    describe "ghcide" $ do
      it "loads RunTests.hs" $
        assertSuccess (Process.proc "./.ghcide" ["tests/RunTests.hs"])
      it "loads module with module dependency" $
        assertSuccess (Process.proc "./.ghcide" ["tests/binary-with-lib/Main.hs"])

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
        it "transitive re-exports do not work #1145" $
          -- https://github.com/tweag/rules_haskell/issues/1145
          assertFailure (bazel ["build", "//tests/package-reexport-transitive"])
        it "doctest failure with foreign import #1559" $
          -- https://github.com/tweag/rules_haskell/issues/1559
          assertFailure (bazel ["build", "//tests/haskell_doctest_ffi_1559:doctest-a"])

    -- Test that the repl still works if we shadow some Prelude functions
    it "repl name shadowing" $ do
      let p (stdout, stderr) = not $ any ("error" `isInfixOf`) [stdout, stderr]
      outputSatisfy p (bazel ["run", "//tests/repl-name-conflicts:lib@repl", "--", "-ignore-dot-ghci", "-e", "stdin"])

    it "Repl works with remote_download_toplevel" $ do
      let p (stdout, stderr) = not $ any ("error" `isInfixOf`) [stdout, stderr]
      withSystemTempDirectory "bazel_disk_cache" $ \tmp_disk_cache -> do
        assertSuccess $ bazel ["run", "//tests/multi_repl:c_only_repl", "--disk_cache=" <> tmp_disk_cache]
        assertSuccess $ bazel ["clean"]
        outputSatisfy p
          (bazel ["run", "//tests/multi_repl:c_only_repl", "--disk_cache=" <> tmp_disk_cache, "--remote_download_toplevel"])

  it "bazel test examples" $ do
    assertSuccess $ (bazel ["build", "//..."]) { Process.cwd = Just "../examples" }
    assertSuccess $ (bazel ["test", "//..."]) { Process.cwd = Just "../examples" }
    assertSuccess $ (bazel ["shutdown"]) { Process.cwd = Just "../examples" }

  it "bazel test tutorial" $ do
    assertSuccess $ (bazel ["build", "//..."]) { Process.cwd = Just "../tutorial" }
    assertSuccess (bazel ["test", "//..."]) { Process.cwd = Just "../tutorial" }
    assertSuccess (bazel ["shutdown"]) { Process.cwd = Just "../tutorial" }

-- * Bazel commands

-- | Returns a bazel command line suitable for CI
-- This should be called with the action as first item of the list. e.g 'bazel ["build", "//..."]'.
bazel :: [String] -> Process.CreateProcess
bazel args = Process.proc "bazel" args

-- | Runs a bazel query and return the list of matching targets
bazelQuery :: String -> SpecM a [String]
bazelQuery q = lines <$> runIO (Process.readProcess "bazel" ["query", q] "")

shutdownBazel :: IO ()
shutdownBazel = do
  putStrLn "MADE IT!"
  assertSuccess (bazel ["shutdown"]) 
  pure ()

-- Generated dependencies for testing the ghcide support
_ghciIDE :: Int
_ghciIDE = a + b
