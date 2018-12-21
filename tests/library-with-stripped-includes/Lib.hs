{-# LANGUAGE CPP #-}
module Lib (x) where

#include "b.h"

x :: Int
x = A + B
