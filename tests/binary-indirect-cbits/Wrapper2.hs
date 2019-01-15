module Wrapper2
  ( module AddOne
  , addOne2
  ) where

import AddOne
import qualified Wrapper

addOne2 = Wrapper.addOne
