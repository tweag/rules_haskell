{-# LANGUAGE CPP #-}
module %{module_name} (create, createFromProgramPath, rlocation) where
import qualified Bazel.Runfiles as RulesHaskellRunfiles
create = RulesHaskellRunfiles.createFromCurrentFile __FILE__
createFromProgramPath = RulesHaskellRunfiles.createFromProgramPathAndCurrentFile (Just __FILE__)
rlocation = RulesHaskellRunfiles.rlocation