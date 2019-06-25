module Main where

import Control.Monad (when)
import GHC.Paths (ghc, ghc_pkg, libdir, docdir)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  let require name value =
        when (null value) $ do
          hPutStrLn stderr (name ++ " should not be empty")
          exitFailure
  require "ghc" ghc
  require "ghc_pkg" ghc_pkg
  require "libdir" libdir
  require "docdir" docdir
