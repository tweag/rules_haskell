module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
  [moduleName, fileName] <- getArgs
  writeFile fileName $
    unlines
      [ unwords ["module", moduleName, "where"],
        "",
        "value :: Int",
        "value = 21"
      ]
