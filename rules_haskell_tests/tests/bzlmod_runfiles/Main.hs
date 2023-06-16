{-# LANGUAGE CPP #-}
module Main where
import Lib

main :: IO ()
main = do
  print $ "file from external main = " <> __FILE__
  print =<< content 

