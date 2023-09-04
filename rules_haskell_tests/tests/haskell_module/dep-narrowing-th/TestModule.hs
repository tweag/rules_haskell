{-# LANGUAGE TemplateHaskell #-}
module TestModule where

import NonModulesTestLib (fooNonModules)
import TestLibModule (foo)
import TestLibModule2 (foo2)

$(return [])

bar :: IO Int
bar = (+ fooNonModules * foo) <$> foo2
