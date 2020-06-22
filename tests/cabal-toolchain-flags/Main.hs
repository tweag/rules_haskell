{-# LANGUAGE CPP #-}

module Main where

#ifdef TESTS_TOOLCHAIN_COMPILER_FLAGS
main :: IO ()
main = pure ()
#endif
