module Main (main) where

import qualified MyModule

main :: IO ()
main = do
  print =<< MyModule.getThing
  MyModule.setThing 123
  print =<< MyModule.getThing
