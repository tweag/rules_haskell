module Options (Options(..), parseArgs) where

import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe)
import Data.Word (Word64)
import System.Exit (ExitCode(ExitFailure), exitSuccess, exitWith)
import System.IO (hPutStrLn, stderr)

data Options = Options
  { optMemoryAllowance :: Word64
  , optPersist :: Bool
  , optServerExecutable :: Maybe FilePath
  , optNumWorkers :: Word64
  }

-- | Yields the memory allowance
parseArgs :: [String] -> IO Options
parseArgs args =
    if elem "--help" args then do
      hPutStrLn stderr helpMessage
      exitSuccess
    else either reportError return $ do
      (rest, memoryAllowance) <- parseMemoryAllowance args
      (rest2, numWorkers) <- parseNumWorkers rest
      Right Options
            { optMemoryAllowance = memoryAllowance
            , optPersist = elem persistentWorkerLabel rest2
            , optServerExecutable = listToMaybe rest2
            , optNumWorkers = numWorkers
            }
  where
    reportError e = do
      hPutStrLn stderr $ "error: " ++ e
      hPutStrLn stderr helpMessage
      exitWith (ExitFailure 1)

persistentWorkerLabel :: String
persistentWorkerLabel = "--persistent_worker"

numWorkersLabel :: String
numWorkersLabel = "--num-workers"

numWorkersDefault :: Word64
numWorkersDefault = 1

memoryAllowanceLabel :: String
memoryAllowanceLabel = "--memory-allowance"

memoryAllowanceDefault :: Word64
memoryAllowanceDefault = 2000

helpMessage :: String
helpMessage = unlines
    [ ""
    , "haskell_module_worker [" ++ memoryAllowanceLabel
      ++ " NUM] [" ++ numWorkersLabel ++ "  NUM] [" ++ persistentWorkerLabel ++ "]"
    , ""
    ] ++
    unlines (map ("    " ++)
      [ memoryAllowanceLabel
      , "    How much memory the worker is allowed to use in MB. Default: "
        ++ show memoryAllowanceDefault
      , ""
      , numWorkersLabel
      , "    How many background workers to use. Default: "
        ++ show numWorkersDefault
      , ""
      , persistentWorkerLabel
      , "    Whether to persist and serve multiple request or just one."
      ]
    )

parseMemoryAllowance :: [String] -> Either String ([String], Word64)
parseMemoryAllowance = parseArgFlag memoryAllowanceLabel memoryAllowanceDefault

parseNumWorkers :: [String] -> Either String ([String], Word64)
parseNumWorkers = parseArgFlag numWorkersLabel numWorkersDefault

parseArgFlag
  :: (Read a, Show a)  => String -> a -> [String] -> Either String ([String], a)
parseArgFlag label def args0 = do
    let (args1, rest) = break (isPrefixOf label) args0
        labelEq = label ++ "="
    case rest of
      lbl : args2 | labelEq `isPrefixOf` lbl ->
        case reads $ drop (length labelEq) lbl of
          [(v,"")] ->
            Right (args1 ++ args2, v)
          r ->
            Left $ "Cannot parse " ++ label ++ ": " ++ show r
      lbl : m : args2 | label == lbl ->
        case reads m of
          [(v,"")] ->
            Right (args1 ++ args2, v)
          r ->
            Left $ "Cannot parse " ++ label ++ ": " ++ show r
      lbl : _ | label /= lbl ->
        Left $ "Unknown flag " ++ lbl
      [] ->
        Right (args0, def)
      _ ->
        Left $ "Missing argument to " ++ label
