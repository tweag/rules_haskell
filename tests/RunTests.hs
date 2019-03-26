{-# OPTIONS -Wall #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Foldable (for_)
import Data.List (isInfixOf, sort)
import System.Exit (ExitCode(..))

import qualified System.Process as Process
import Test.Hspec.Core.Spec (SpecM)
import Test.Hspec (hspec, it, describe, runIO, shouldSatisfy, expectationFailure)

main :: IO ()
main = hspec $ do
  it "bazel lint" $ do
    assertSuccess (bazel ["run", "//:buildifier"])

  it "bazel test" $ do
    assertSuccess (bazel ["test", "//...", "--build_tests_only"])

  it "haddock links" $ do
    -- Test haddock links
    -- All haddock tests are stored inside //tests/haddock
    -- Temporaries files appears inside /doc-.... outputs and are ignored

    -- the copy / chmod is here to workaround the fact that
    -- linkchecker is dropping privileges to "nobody" user if called
    -- from root, which is the case on CI.
    assertSuccess (safeShell
      [ "bazel build --config=ci //tests/haddock/..."
      , "pwd=$(pwd)"
      , "cd $(mktemp -d)"
      , "cp -r $pwd/bazel-ci-bin/tests/haddock ."
      , "chmod -R o+r ."
      , "linkchecker . --ignore-url=/doc-"
      ])

  it "bazel test prof" $ do
    assertSuccess (bazel ["test", "-c", "dbg", "//...", "--build_tests_only"])

  describe "repl" $ do
    it "for libraries" $ do
      -- Test whether building of repl forces all runtime dependencies by itself:
      -- TODO(Profpatsch) remove clean once repl uses runfiles
      -- because otherwise the test runner succeeds if the first test fails
      assertSuccess (bazel ["clean"])

      assertSuccess (bazel ["run", "//tests/repl-targets:hs-lib@repl", "--", "-ignore-dot-ghci", "-e", "show (foo 10) ++ bar ++ baz ++ gen"])
      assertSuccess (bazel ["run", "//tests/repl-targets:hs-lib-bad@repl", "--", "-ignore-dot-ghci", "-e", "1 + 2"])

    it "for binaries" $ do
      -- Test whether building of repl forces all runtime dependencies by itself:
      -- TODO(Profpatsch) remove clean once repl uses runfiles
      assertSuccess (bazel ["clean"])

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

  it "startup script" $ do
    assertSuccess (safeShell [
        "pwd=$(pwd)"
      , "cd $(mktemp -d)"
      , "$pwd/start"

      -- Copy the bazel configuration, this is only useful for CI
      , "mkdir tools"
      , "cp $pwd/.bazelrc .bazelrc"

      -- Set Nixpkgs in environment variable to avoid hardcoding it in
      -- start script itself
      , "NIX_PATH=nixpkgs=$pwd/nixpkgs/default.nix \
        \bazel fetch \
        \--config=ci \
        \--override_repository=io_tweag_rules_haskell=$pwd \
        \//..."
      ])

  describe "failures" $ do
    all_failure_tests <- bazelQuery "kind(rule, //tests/failures/...) intersect attr('tags', 'manual', //tests/failures/...)"

    for_ all_failure_tests $ \test -> do
      it test $ do
        assertFailure (bazel ["build", "test"])

  -- Test that the repl still works if we shadow some Prelude functions
  it "repl name shadowing" $ do
    let p (stdout, stderr) = not $ any ("error" `isInfixOf`) [stdout, stderr]
    outputSatisfy p (bazel ["run", "//tests/repl-name-conflicts:lib@repl", "--", "-ignore-dot-ghci", "-e", "stdin"])

  it "bazel test examples" $ do
    assertSuccess (bazel ["test", "@io_tweag_rules_haskell_examples//..."])

  it "bazel test tutorial" $ do
    assertSuccess (bazel ["test", "@io_tweag_rules_haskell_tutorial//..."])

  describe "Hazel" $ do
    it "bazel test" $ do
      assertSuccess (bazel ["test", "@ai_formation_hazel//..."])
    for_ hazelPackages $ \pkg -> do
      it ("builds " ++ pkg) $ do
        assertSuccess (bazel ["build", "@haskell_" ++ pkg ++ "//..."])

      where
        hazelPackages =
          [ "aeson"
          , "aeson__extra"
          , "cassava"
          , "conduit"
          , "entropy"
          , "fuzzyset"
          , "htaglib"
          , "http__client"
          , "lens"
          , "network"
          , "postgresql__libpq"
          , "pretty__show"
          , "text__metrics"
          , "unix__compat"
          , "unix__time"
          -- , "wai__app__static"
          , "zlib"
          ]

-- * Bazel commands

-- | Returns a bazel command line suitable for CI
-- This should be called with the action as first item of the list. e.g 'bazel ["build", "//..."]'.
bazel :: [String] -> Process.CreateProcess
-- Note: --config=ci is intercalated between the action and the list
-- of arguments. It should appears after the action, but before any
-- @--@ following argument.
bazel (command:args) = Process.proc "bazel" (command:"--config=ci":args)
bazel [] = Process.proc "bazel" []

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

-- | The command must success
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
