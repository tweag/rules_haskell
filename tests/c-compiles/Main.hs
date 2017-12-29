module Main (main) where

import Lib (ten)

main :: IO ()
main = if ten == 10
       then putStrLn "c-compiles"
       else error $ "Unexpected result from Lib.ten: " ++ show ten
