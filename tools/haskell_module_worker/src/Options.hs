module Options (Options(..), parseArgs) where

import Data.Word (Word64)
import System.Exit (ExitCode(ExitFailure), exitSuccess, exitWith)
import System.IO (hPutStrLn, stderr)

data Options = Options
  { optPersist :: Bool
  }

-- | Yields the memory allowance
parseArgs :: [String] -> IO Options
parseArgs args =
    if elem "--help" args then do
      hPutStrLn stderr helpMessage
      exitSuccess
    else
      return Options
        { optPersist = elem persistentWorkerLabel args
        }

persistentWorkerLabel :: String
persistentWorkerLabel = "--persistent_worker"

helpMessage :: String
helpMessage = unlines
    [ ""
    , "haskell_module_worker [" ++ persistentWorkerLabel ++ "]"
    , ""
    ] ++
    unlines (map ("    " ++)
      [ persistentWorkerLabel
      , "    Whether to persist and serve multiple request or just one."
      ]
    )
