module TestModule(bar, foo2, depFoo) where

import TestLibModule (foo)
import TestLibModule2 (foo2, depFoo)

bar :: Int
bar = 2 * foo + foo2
