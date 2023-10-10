import Test.Hspec (hspec, it)
import IntegrationTesting
import qualified System.Process as Process
import Data.Foldable (for_)

main = hspec $ do
  it "bazel test recompilation" $ do
    (newWorkspaceDir, outputUserRoot) <- setupWorkspace
    bazel <- bazelCmd newWorkspaceDir outputUserRoot
    let in_recompilation f = f {Process.cwd = Just newWorkspaceDir}
    let recompilation_is_not_triggered_by_patch :: Int -> IO ()
        recompilation_is_not_triggered_by_patch i = do
          assertSuccess $
            bazel  ["build", "//:basic_modules"]
          -- When a patch is applied,
          print "Applying patch"
          assertSuccess $ in_recompilation $
            Process.proc "git" ["apply", "patch" ++ show i]
          -- it triggers the recompilation of some modules,
          print "Patch applied"
          assertSuccess $ in_recompilation $
            bazel ["build", "//:basic_modules", "--execution_log_json_file=logfile.json"]
          -- but the recompilation avoidance mechanism should guarantee that the module `C` is not recompiled.
          assertFailure $ in_recompilation $
            Process.proc "grep" ["C.hs", "logfile.json"]
          -- We finish by reverting the changes made by the patch application.
          assertSuccess $ in_recompilation $
            Process.proc "git" ["apply", "-R", "patch" ++ show i]
    for_ [1..4] recompilation_is_not_triggered_by_patch
