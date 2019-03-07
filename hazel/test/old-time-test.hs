module Main (main) where

main :: IO ()
main = return ()

-- Force the dynamic linker to load all symbols from its dependencies
{-# ANN main () #-}
