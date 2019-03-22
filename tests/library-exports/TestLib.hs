module TestLib (testMessage) where

import TestSubLib (messageEnd)
import SubLib.Map

testMessage :: String
testMessage = "hello " ++ messageEnd
