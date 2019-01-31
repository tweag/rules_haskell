module Lib (printValue) where

import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)

getValue :: Monad m => ExceptT e m Int
getValue = pure 42

printValue :: IO ()
printValue = void $ runExceptT $ do
  value <- getValue
  lift $ print value
