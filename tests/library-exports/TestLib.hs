module TestLib (testMessage) where

import TestSubLib (messageEnd)
import SubLib.List

testMessage :: String
testMessage = "hello " ++ SubLib.List.nub messageEnd
