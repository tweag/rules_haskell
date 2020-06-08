{-# LANGUAGE TemplateHaskell #-}

module LibB where

import LibA

checkValue :: Bool
checkValue = $(value) == (42 :: Int)
