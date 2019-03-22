{-# LANGUAGE TemplateHaskell #-}

module Lib (bar) where

import TH (foo)

bar :: IO ()
bar = $foo
