{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import qualified Language.Haskell.TH as TH (runIO)
import qualified Language.Haskell.TH.Syntax as TH (lift)
import           System.Environment (lookupEnv)

main :: IO ()
main = putStrLn $(
  let ensureClassPath :: IO String
      ensureClassPath = lookupEnv "CLASSPATH" >>= \case
        Nothing -> error "CLASSPATH not set when it was expected to be."
        Just "" -> error "CLASSPATH empty when it was expected to have content."
        Just cpath -> pure $ "java-classpath at compile time: " ++ cpath
  in TH.runIO ensureClassPath >>= TH.lift
  )
