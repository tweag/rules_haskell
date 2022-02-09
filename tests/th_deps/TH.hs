{-# LANGUAGE TemplateHaskell #-}
module TH(valueQ) where

import Language.Haskell.TH
import LibA(hype)

foreign import ccall "c_add_one" c_add_one' :: Int -> Int

valueQ :: Q Exp
valueQ = [| hype (show (c_add_one' 41)) |]
