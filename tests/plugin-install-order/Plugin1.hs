module Plugin1 (plugin) where

import Control.Monad (when)
import GHC.Driver.Plugins
import System.Process (readProcess)

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _args todo = do
  pure $ todo ++ [CoreDoPluginPass "192839898888299" pure]
