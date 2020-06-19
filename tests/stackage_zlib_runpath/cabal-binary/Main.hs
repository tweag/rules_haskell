{-# LANGUAGE OverloadedStrings #-}
module Main where

import Codec.Compression.Zlib (compress)

main :: IO ()
main = do
  case compress "" of
    _ -> pure ()
