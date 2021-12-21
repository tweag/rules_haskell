{-# LANGUAGE TemplateHaskell #-}
module LibExample where

import Prelude ((.), putStrLn)
import Language.Haskell.TH

exampleFunction =
    $(do runIO (putStrLn "Hello, ")
         [| putStrLn "Library!" |]
     )
