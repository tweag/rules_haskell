{-# LANGUAGE CPP #-}
module Lib (x) where

#include "tests/library-with-includes/a.h"
#include "tests/library-with-includes/b.h"

x :: Int
x = A + B
