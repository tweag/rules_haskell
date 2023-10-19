{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import           Control.Exception (SomeException, try)
import qualified Language.Haskell.TH as TH (runIO)
import qualified Language.Haskell.TH.Syntax as TH (lift)
import           System.Environment (lookupEnv)
import           System.IO (IOMode(..), hClose, openBinaryFile)

main :: IO ()
main = putStrLn $(
  let ensureClassPath :: IO String
      ensureClassPath = lookupEnv "CLASSPATH" >>= \case
        Nothing -> error "CLASSPATH not set when it was expected to be."
        Just "" -> error "CLASSPATH empty when it was expected to have content."
        Just cpath -> do
          let path = takeWhile (/= ':') $ dropWhile (== ':') cpath
          -- Tests that we can read files in the classpath.
          try (openBinaryFile path ReadMode >>= hClose) >>= \case
            Left e -> pure $ unlines
              [ "can't read the first file in the classpath: "
              , path
              , show (e :: SomeException)
              ]
            Right () ->
              pure $ "java-classpath at compile time: " ++ cpath
  in TH.runIO ensureClassPath >>= TH.lift
  )
