{-# LANGUAGE TemplateHaskell #-}
module TestModule where

import TestLibModule (foo)
import TestLibModule2 (foo2)

$(return [])

bar :: IO Int
bar = (+ 2 * foo) <$> foo2
