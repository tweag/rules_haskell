module Options (Options(..), parseArgs) where

import Data.Word (Word64)
import System.Exit (ExitCode(ExitFailure), exitSuccess, exitWith)
import System.IO (hPutStrLn, stderr)

data Options = Options
  { optMemoryAllowance :: Word64
  , optPersist :: Bool
  }

-- | Yields the memory allowance
parseArgs :: [String] -> IO Options
parseArgs args =
    if elem "--help" args then do
      hPutStrLn stderr helpMessage
      exitSuccess
    else case parseMemoryAllowance args of
      Right (rest, memoryAllowance) -> return Options
        { optMemoryAllowance = memoryAllowance
        , optPersist = elem persistentWorkerLabel rest
        }
      Left e -> do
        hPutStrLn stderr $ "error: " ++ e
        hPutStrLn stderr helpMessage
        exitWith (ExitFailure 1)

persistentWorkerLabel :: String
persistentWorkerLabel = "--persistent_worker"

memoryAllowanceLabel :: String
memoryAllowanceLabel = "--memory-allowance"

memoryAllowanceDefault :: Word64
memoryAllowanceDefault = 2000

helpMessage :: String
helpMessage = unlines
    [ ""
    , "haskell_module_worker [" ++ memoryAllowanceLabel
      ++ "] [" ++ persistentWorkerLabel ++ "]"
    , ""
    ] ++
    unlines (map ("    " ++)
      [ memoryAllowanceLabel
      , "    How much memory the worker is allowed to use in MB. Default: "
        ++ show memoryAllowanceDefault
      , ""
      , persistentWorkerLabel
      , "    Whether to persist and serve multiple request or just one."
      ]
    )

parseMemoryAllowance :: [String] -> Either String ([String], Word64)
parseMemoryAllowance args0 = do
    let (args1, rest) = break (== memoryAllowanceLabel) args0
    case rest of
      _:m:args2 -> case reads m of
        [(memoryAllowance,"")] ->
          Right (args1 ++ args2, memoryAllowance)
        r ->
          Left $ "Cannot parse " ++ memoryAllowanceLabel ++ ": " ++ show r
      [] ->
        Right (args0, memoryAllowanceDefault)
      _ ->
        Left $ "Missing argument to " ++ memoryAllowanceLabel


