{-# LANGUAGE TemplateHaskell #-}

module Foo (foo) where

import FooTH (embedFile)

foo :: String
foo = $(embedFile "tests/extra-source-files/file.txt") ++ "!"
