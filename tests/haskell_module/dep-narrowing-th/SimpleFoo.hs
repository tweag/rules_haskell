module SimpleFoo where

import System.IO.Temp

foo :: IO Int
foo = withSystemTempDirectory "test" $ \_ -> return 23
