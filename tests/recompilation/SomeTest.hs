import Test.Hspec (hspec, it)
import IntegrationTesting

main = hspec $ do
  it "bazel test recompilation" $
    let in_recompilation f = f {Process.cwd = Just "./tests/recompilation"}
    in
    let recompilation_is_not_triggered_by_patch :: Int -> IO ()
        recompilation_is_not_triggered_by_patch i = do
          assertSuccess $
            bazel ["build", "//:basic_modules"]
          -- When a patch is applied,
          assertSuccess $ in_recompilation $
            Process.proc "git" ["apply", "patch" ++ show i]
          -- it triggers the recompilation of some modules,
          assertSuccess $ in_recompilation $
            bazel ["build", "//:basic_modules", "--execution_log_json_file=logfile.json"]
          -- but the recompilation avoidance mechanism should guarantee that the module `C` is not recompiled.
          assertFailure $ in_recompilation $
            Process.proc "grep" ["C.hs", "logfile.json"]
          -- We finish by reverting the changes made by the patch application.
          assertSuccess $ in_recompilation $
            Process.proc "git" ["checkout", "src/*.hs"]
    in
    for_ [1..4] recompilation_is_not_triggered_by_patch
