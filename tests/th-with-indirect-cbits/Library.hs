{-# LANGUAGE TemplateHaskell #-}

module Library where

import HsLibDirect

value :: Int
value = $(direct)
