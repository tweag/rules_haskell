{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import TH

main :: IO ()
main = putStrLn $foo
