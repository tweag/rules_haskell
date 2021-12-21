{-# LANGUAGE TemplateHaskell #-}
module Main where

import Prelude ((.), putStrLn)
import Language.Haskell.TH
import LibExample (exampleFunction)

main = $(do runIO (putStrLn "Hello, ")
            [| exampleFunction |]
        )
