{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Lib (tenLit)

ten = $(tenLit)

main :: IO ()
main = if ten == 10
       then putStrLn "c-compiles"
       else error $ "Unexpected result from Lib.ten: " ++ show ten
