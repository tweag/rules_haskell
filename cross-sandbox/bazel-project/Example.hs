{-# LANGUAGE TemplateHaskell #-}
module Main where

import Codec.Compression.Zlib (compress, decompress)
import Prelude ((.), putStrLn)
import Language.Haskell.TH

main = $(do runIO (putStrLn "Hello, ")
            [| putStrLn "World!" |]
        )

slowId = decompress . compress
