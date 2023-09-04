module SimpleFoo where

import TestLibModule1 (foo1)
import System.IO.Temp

foo :: IO Int
foo = withSystemTempDirectory "test" $ \_ -> return foo1
