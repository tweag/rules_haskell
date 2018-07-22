{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Provide where
import Data.Map
newtype MyMap a = MyMap (Map String a)
    deriving (Functor)
