module Plugin2 (plugin) where

import Control.Monad (when)
import GHC.Driver.Plugins
import GHC.Core.Opt.Monad
import System.Process (readProcess)

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _args todo = do
  if not $ ok todo
    then error "Plugin1 not applied before Plugin2 - see /tests/plugin-install-order"
    else pure todo

ok todo = case todo of
  [] -> False
  xs -> case last xs of
    CoreDoPluginPass str _ -> str == "192839898888299"
    _ -> False
