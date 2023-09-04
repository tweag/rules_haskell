{-# LANGUAGE CPP #-}

module Main where

import Data.ByteString

#ifdef TESTS_TOOLCHAIN_COMPILER_FLAGS
main = print ("hello" :: ByteString)
#endif

