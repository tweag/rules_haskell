{-# LANGUAGE TemplateHaskell #-}
module Foo where

-- This is here just to trigger template haskell
blork = $([| 1 + 1 |])
