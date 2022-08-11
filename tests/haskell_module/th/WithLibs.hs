{-# LANGUAGE TemplateHaskell #-}

module WithLibs where

import qualified Codec.Compression.GZip as GZip
import Control.Monad (replicateM)
import Root

import Data.PhoneNumber
import qualified Data.Vector as Vector
import Language.Haskell.TH (runIO)

runIO (replicateM root (return Vector.empty)) >> return []

main = GZip.compress

newtype PhoneNumberTest = PhoneNumberTest PhoneNumber
