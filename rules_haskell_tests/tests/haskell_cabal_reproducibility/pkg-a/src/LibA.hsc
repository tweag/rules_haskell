module LibA (double) where

#include "myclib.h"

foreign import ccall "myclib.h add" c_add :: Int -> Int -> Int

double :: Int -> Int
double a = c_add a a
