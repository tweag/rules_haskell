module Plugin (plugin) where

import Control.Monad (when)
import GhcPlugins

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install [arg] todo = do
  when ('$' `elem` arg) $
    fail "Make variable not expanded."
  return todo
