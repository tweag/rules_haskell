module Options (parseArgs) where

import Data.Word (Word64)
import System.Exit (ExitCode(ExitFailure), exitSuccess, exitWith)
import System.IO (hPutStrLn, stderr)

-- | Parse options intended for the worker
--
-- Yields the options intended for GHC
parseArgs :: [String] -> IO ([String], Word64)
parseArgs args =
    if elem "--help" args then do
      hPutStrLn stderr helpMessage
      exitSuccess
    else case parseMemoryAllowance args of
      Right r -> return r
      Left e -> do
        hPutStrLn stderr $ "error: " ++ e
        hPutStrLn stderr helpMessage
        exitWith (ExitFailure 1)

memoryAllowanceLabel :: String
memoryAllowanceLabel = "--memory-allowance"

memoryAllowanceDefault :: Word64
memoryAllowanceDefault = 2000

helpMessage :: String
helpMessage = unlines
    [ ""
    , "haskell_module_worker [" ++ memoryAllowanceLabel ++ "] <ghc_opts>"
    , ""
    ] ++
    unlines (map ("    " ++)
      [ memoryAllowanceLabel
      , "    How much memory the worker is allowed to use in MB. Default: "
        ++ show memoryAllowanceDefault
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


