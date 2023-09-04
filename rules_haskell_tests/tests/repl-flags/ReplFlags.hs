{-# LANGUAGE CPP #-}
module Main where

import Data.ByteString

-- Ensure that `compiler-flags` are correctly set
#ifdef TESTS_TOOLCHAIN_COMPILER_FLAGS
main = print "hello"
#endif

-- Ensure that `repl_ghci_args` are correctly set
-- OverloadedString is passed by toolchain
-- The CPP constant is unset by toolchain and set by rule, so ordering must be ensured
#ifdef TESTS_TOOLCHAIN_REPL_FLAGS
foo = ("world" :: ByteString)
#endif

