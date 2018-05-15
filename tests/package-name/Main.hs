{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
module Main (main) where

import Control.Monad (when)

-- Test the PackageImports extension.
import "lib-aZ" Lib (foo)

-- Test macros that GHC creates automatically based on the package version.
versionHighEnoughTest, versionTooLowTest :: Bool
#if MIN_VERSION_lib_aZ(1,2,3)
versionHighEnoughTest = True
#else
versionHighEnoughTest = False
#endif

#if MIN_VERSION_lib_aZ(1,2,4)
versionTooLowTest = False
#else
versionTooLowTest = True
#endif

check :: (Show a, Eq a) => a -> a -> IO ()
check x y = when (x /= y) $ error $ "Failed check: " ++ show (x, y)

main :: IO ()
main = do
    check foo 42
    check VERSION_lib_aZ "1.2.3.4"
    check versionHighEnoughTest True
    check versionTooLowTest True
