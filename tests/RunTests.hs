{-# OPTIONS -Wall #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Foldable (for_)
import Data.List (isInfixOf, sort)
import System.Exit (ExitCode(..))
import System.Info (os)

import qualified System.Process as Process
import Test.Hspec.Core.Spec (SpecM)
import Test.Hspec (hspec, it, describe, runIO, shouldSatisfy, expectationFailure)

main :: IO ()
main = hspec $ do
  it "bazel lint" $ do
    assertSuccess (bazel ["run", "//:buildifier"])

  it "bazel test" $ do
    assertSuccess (bazel ["test", "//...", "--build_tests_only"])

  it "bazel test prof" $ do
    -- In .circleci/config.yml we specify --test_tag_filters
    -- -dont_test_on_darwin. However, specifiying --test_tag_filters
    -- -requires_dynamic here alone would override that filter. So,
    -- we have to duplicate that filter here.
    let tagFilter | os == "darwin" = "-dont_test_on_darwin,-requires_dynamic"
                  | otherwise      = "-requires_dynamic"
    assertSuccess (bazel ["test", "-c", "dbg", "//...", "--build_tests_only", "--test_tag_filters", tagFilter])

  it "bazel build worker" $ do
    assertSuccess (bazel ["build", "//tools/worker:bin"])

  describe "repl" $ do
    it "for libraries" $ do
      assertSuccess (bazel ["run", "//tests/repl-targets:hs-lib@repl", "--", "-ignore-dot-ghci", "-e", "show (foo 10) ++ bar ++ baz ++ gen"])
      assertSuccess (bazel ["run", "//tests/repl-targets:hs-lib-bad@repl", "--", "-ignore-dot-ghci", "-e", "1 + 2"])

    it "for binaries" $ do
      assertSuccess (bazel ["run", "//tests/repl-targets:hs-bin@repl", "--", "-ignore-dot-ghci", "-e", ":main"])

      assertSuccess (bazel ["run", "//tests/binary-indirect-cbits:binary-indirect-cbits@repl", "--", "-ignore-dot-ghci", "-e", ":main"])

    -- Test `compiler_flags` from toolchain and rule for REPL
    it "compiler flags" $ do
      assertSuccess (bazel ["run", "//tests/repl-flags:compiler_flags@repl", "--", "-ignore-dot-ghci", "-e", ":main"])

    -- Test `repl_ghci_args` from toolchain and rule for REPL
    it "repl flags" $ do
      assertSuccess (bazel ["run", "//tests/repl-flags:repl_flags@repl", "--", "-ignore-dot-ghci", "-e", "foo"])

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
    it "allows to manually load modules" $ do
      assertSuccess (bazel ["run", "//tests/multi_repl:c_multi_repl", "--", "-ignore-dot-ghci", "-e", ":load BC.C", "-e", "c"])

  describe "failures" $ do
    -- Make sure not to include haskell_repl (@repl) or alias (-repl) targets
    -- in the query. Those would not fail under bazel test.
    all_failure_tests <- bazelQuery "kind('haskell_library|haskell_binary|haskell_test', //tests/failures/...) intersect attr('tags', 'manual', //tests/failures/...)"

    for_ all_failure_tests $ \test -> do
      it test $ do
        assertFailure (bazel ["build", test])

  -- Test that the repl still works if we shadow some Prelude functions
  it "repl name shadowing" $ do
    let p (stdout, stderr) = not $ any ("error" `isInfixOf`) [stdout, stderr]
    outputSatisfy p (bazel ["run", "//tests/repl-name-conflicts:lib@repl", "--", "-ignore-dot-ghci", "-e", "stdin"])

  it "bazel test examples" $ do
    assertSuccess $ (bazel ["build", "//..."]) { Process.cwd = Just "./examples" }
    assertSuccess $ (bazel ["test", "//..."]) { Process.cwd = Just "./examples" }

  it "bazel test tutorial" $ do
    assertSuccess $ (bazel ["build", "//..."]) { Process.cwd = Just "./tutorial" }
    assertSuccess (bazel ["test", "//..."]) { Process.cwd = Just "./tutorial" }

-- * Bazel commands

-- | Returns a bazel command line suitable for CI
-- This should be called with the action as first item of the list. e.g 'bazel ["build", "//..."]'.
bazel :: [String] -> Process.CreateProcess
bazel args = Process.proc "bazel" args

-- | Runs a bazel query and return the list of matching targets
bazelQuery :: String -> SpecM a [String]
bazelQuery q = lines <$> runIO (Process.readProcess "bazel" ["query", q] "")

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

-- | Execute in a sub shell the list of command
-- This will fail if any of the command in the list fail
safeShell :: [String] -> Process.CreateProcess
safeShell l = Process.shell (unlines ("set -e":l))

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
