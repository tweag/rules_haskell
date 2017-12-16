module TestLib (testMessage) where

import TestSubLib (messageEnd)

testMessage :: String
testMessage = "hello " ++ messageEnd
