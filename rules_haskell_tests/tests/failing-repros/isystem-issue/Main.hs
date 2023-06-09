{-# LANGUAGE CPP #-}

#include "ghcautoconf.h"

import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

main :: IO ()
main = do
#ifndef RULES_HASKELL_ISYSTEM_TEST
  hPutStrLn stderr "Included wrong ghcautoconf.h"
  exitFailure
#else
  pure ()
#endif
