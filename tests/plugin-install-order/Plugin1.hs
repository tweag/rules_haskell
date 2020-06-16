module Plugin1 (plugin) where

import Control.Monad (when)
import GhcPlugins
import System.Process (readProcess)

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install [arg] todo = do
  pure $ todo ++ [CoreDoPluginPass "192839898888299" pure]
