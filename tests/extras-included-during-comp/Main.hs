{-# LANGUAGE CPP #-}
module Main (main) where

-- This lives under "includes" so we're testing that bazel passes in
-- the appropriate include flag.
#include "main_definition"
