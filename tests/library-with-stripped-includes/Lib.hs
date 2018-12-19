{-# LANGUAGE CPP #-}
module Lib (x) where

#include "a.h"
#include "b.h"

x :: Int
x = A + B
