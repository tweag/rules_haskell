{-# LANGUAGE CPP #-}

module Lib (answer) where

answer :: Int
#if __GLASGOW_HASKELL__ >= 900
answer = 42
#else
answer = 0
#endif
