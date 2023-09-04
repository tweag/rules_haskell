{-# LANGUAGE CPP #-}

module Main where

import Data.ByteString

#ifdef TESTS_TOOLCHAIN_CABALOPTS
main :: IO ()
main = print ("hello" :: ByteString)
#endif
