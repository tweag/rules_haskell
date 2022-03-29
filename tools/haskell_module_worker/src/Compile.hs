{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE CPP #-}

-- | This module implements a function to compile modules individually
-- in a single GHC session, which should avoid the cost of starting
-- the compiler multiple times.
module Compile (CompileM, Status(..), compile, runSession) where

import Control.Exception (throwIO)
import Control.Monad (guard, when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (ReaderT(..))
import Data.IORef
import Data.List (intercalate, isPrefixOf, partition)
import Data.Maybe
import System.FilePath
import System.IO (hPutStrLn, stderr)

import GHC hiding (Succeeded)
import GHC.Paths (libdir)
import DynFlags
  ( CompilerInfo
  , FlagSpec
  , LinkerInfo
  , Option(..)
  , WarningFlag
  , WarnReason(..)
  , defaultFatalMessager
  , defaultFlushOut
  , flagSpecFlag
  , flagSpecName
  , warningGroups
  , warningHierarchies
  , wWarningFlags
  )
import DriverPhases
import DriverPipeline (compileFile)
import ErrUtils
  ( MsgDoc
  , getCaretDiagnostic
  , mkLocMessageAnn
  , pprErrMsgBagWithLoc
  )
import HscTypes (handleFlagWarnings, hsc_dflags, srcErrorMessages)
import Panic (fromException, try)
import PlainPanic (PlainGhcException(PlainPanic))
import Outputable
import Util

data Status
  = NonHaskellInputs [String]        -- Can only compile Haskell modules
  | NonOneShotCompilation            -- Can only compile in OneShot mode
  | CompileErrors [String] [String]  -- Logs and compilation errors
  | Succeeded [String]               -- Logs (include warnings)
  deriving Show

isErrorStatus :: Status -> Bool
isErrorStatus = \case Succeeded{} -> False; _ -> True

-- | We store the initial state of dflags to avoid interference
-- between different compilation requests.
--
-- Unfortunately, DynFlags carries mutable state, which means that
-- we sometimes need to capture the values of some IORefs in order
-- to restore them later.
data DynFlagsState = DynFlagsState
  { initialDFlags :: DynFlags
  , initialRTLDInfo :: Maybe LinkerInfo
  , initialRTCCInfo :: Maybe CompilerInfo
  , initialCanGenerateDynamicToo :: Bool
  }

captureDynFlagsState :: DynFlags -> IO DynFlagsState
captureDynFlagsState dflags = do
    initialRTLDInfo <- readIORef (rtldInfo dflags)
    initialRTCCInfo <- readIORef (rtccInfo dflags)
    initialCanGenerateDynamicToo <- readIORef (canGenerateDynamicToo dflags)
    return DynFlagsState
      { initialDFlags = dflags
      , initialRTCCInfo
      , initialRTLDInfo
      , initialCanGenerateDynamicToo
      }

getDynFlagsFromState :: DynFlagsState -> IO DynFlags
getDynFlagsFromState DynFlagsState
      { initialDFlags
      , initialRTCCInfo
      , initialRTLDInfo
      , initialCanGenerateDynamicToo
      } = do
    writeIORef (rtccInfo initialDFlags) initialRTCCInfo
    writeIORef (rtldInfo initialDFlags) initialRTLDInfo
    writeIORef (canGenerateDynamicToo initialDFlags) initialCanGenerateDynamicToo
    return initialDFlags

-- | Monad used for compiling.
--
-- It carries the initial DynFlags to reconfigure the session
-- afresh for each request.
newtype CompileM a = CompileM { unCompileM :: ReaderT DynFlagsState Ghc a }
  deriving (Applicative, Functor, Monad, MonadIO)

-- | Starts a session where multiple compile calls can be done
runSession :: CompileM a -> IO a
runSession action =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $
    runGhc (Just libdir) $ do
      dflagsState <- getSessionDynFlags >>= liftIO . captureDynFlagsState
      runReaderT (unCompileM action) dflagsState

compile :: [String] -> Int -> CompileM Status
compile flags workerVerbosity = do

    when (workerVerbosity > 0) $
      liftIO $ hPutStrLn stderr $
        "Compiling with flags: " ++ unwords (map show flags)

    let (cflags, flags') = partition ("-c" ==) flags
    if null cflags then
      return NonOneShotCompilation
    else do
      st <- compileOneShot flags' workerVerbosity
      when (workerVerbosity > 0) $
        liftIO $ hPutStrLn stderr $ "Finished with status: " ++ show st
      return st

-- | Compiles with the given flags and source files
--
-- Some of this function is copied from @ghc/Main.hs@.
-- https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-8.10.7-release/ghc/Main.hs#L151
compileOneShot :: [String] -> Int -> CompileM Status
compileOneShot flags workerVerbosity = CompileM $ ReaderT $ \dflagsState0 -> do

    dflags0 <- liftIO (getDynFlagsFromState dflagsState0)

    -- Parse flags
    (dflags2, fileish_args, dynamicFlagWarnings) <-
      parseDynamicFlags dflags0 (map noLoc flags)

    -- Normalize paths
    let
      normalise_hyp fp
        | strt_dot_sl && "-" `isPrefixOf` nfp = cur_dir ++ nfp
        | otherwise                           = nfp
        where
# if defined(mingw32_HOST_OS)
          strt_dot_sl = "./" `isPrefixOf` fp || ".\\" `isPrefixOf` fp
# else
          strt_dot_sl = "./" `isPrefixOf` fp
# endif
          cur_dir = '.' : [pathSeparator]
          nfp = normalise fp
      normal_fileish_paths = map (normalise_hyp . unLoc) fileish_args
      (srcs, objs)         = partition_args normal_fileish_paths [] []

    logsRef <- liftIO $ newIORef []

    let
      dflags3 = dflags2
        { ldInputs = map (FileOption "") objs ++ ldInputs dflags2
        , ghcMode = OneShot
        , log_action = renderLog $ \msg -> do
            when (workerVerbosity > 0) $
              hPutStrLn stderr msg
            modifyIORef logsRef (msg:)
        }

    _ <- setSessionDynFlags dflags3

    st0 <- collectStatus logsRef $ handleFlagWarnings dflags3 dynamicFlagWarnings
    if isErrorStatus st0 then
      return st0
    else
      doCompile logsRef srcs

-- Compiles the given source files after flags have been used
-- to setup the session
doCompile :: IORef [String] -> [(String, Maybe Phase)] -> Ghc Status
doCompile logsRef srcs = do
    let (hs_srcs, non_hs_srcs) = partition isHaskellishTarget srcs
    if null (non_hs_srcs) then do
      hsc_env <- GHC.getSession
      collectStatus logsRef $
        mapM_ (compileFile hsc_env StopLn) hs_srcs
    else
      return $ NonHaskellInputs $ map fst non_hs_srcs

-- | Creates a status from the exceptions thrown by the given action
-- and the logs in the given 'IORef'.
collectStatus :: IORef [String] -> IO () -> Ghc Status
collectStatus logsRef action = do
    hsc_env <- GHC.getSession
    liftIO $ do
      ee <- try action
      logs <- reverse <$> readIORef logsRef
      case ee of
        Left se -> case fromException se of
          Just e -> do
            return $
              CompileErrors logs $
              map (showSDoc $ hsc_dflags hsc_env) $
              pprErrMsgBagWithLoc $
              srcErrorMessages e
          Nothing -> case fromException se of
            Just e | isPlainPanic e ->
              return $
                CompileErrors logs [show (e :: PlainGhcException)]
            _ -> case fromException se of
              Just e | not (isPanic e) ->
                return $
                  CompileErrors logs [show (e :: GhcException)]
              _ ->
                throwIO se
        Right _ ->
          return (Succeeded logs)
  where
    isPanic (Panic{}) = True
    isPanic (PprPanic{}) = True
    isPanic _ = False

    isPlainPanic (PlainPanic{}) = True
    isPlainPanic _ = False

-- | Copied from @ghc/Main.hs@
-- https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-8.10.7-release/ghc/Main.hs#L297
partition_args :: [String] -> [(String, Maybe Phase)] -> [String]
               -> ([(String, Maybe Phase)], [String])
partition_args [] srcs objs = (reverse srcs, reverse objs)
partition_args ("-x":suff:args) srcs objs
  | "none" <- suff      = partition_args args srcs objs
  | StopLn <- phase     = partition_args args srcs (slurp ++ objs)
  | otherwise           = partition_args rest (these_srcs ++ srcs) objs
        where phase = startPhase suff
              (slurp,rest) = break (== "-x") args
              these_srcs = zip slurp (repeat (Just phase))
partition_args (arg:args) srcs objs
  | looks_like_an_input arg = partition_args args ((arg,Nothing):srcs) objs
  | otherwise               = partition_args args srcs (arg:objs)

looks_like_an_input :: String -> Bool
looks_like_an_input m =  isSourceFilename m
                      || looksLikeModuleName m
                      || "-" `isPrefixOf` m
                      || not (hasExtension m)

-- Some of this function is copied from @compiler/main/DynFlags.hs@.
-- https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-8.10.7-release/compiler/main/DynFlags.hs#L2216
renderLog
  :: (String -> IO a)
  -> DynFlags
  -> WarnReason
  -> Severity
  -> SrcSpan
  -> PprStyle
  -> MsgDoc
  -> IO a
renderLog k dflags reason severity srcSpan pprStyle msg
    = (>>= k) $ case severity of
      SevWarning     -> printWarns
      SevError       -> printWarns
      _      -> return $ showSDoc dflags msg
    where
      message = mkLocMessageAnn flagMsg severity srcSpan msg

      printWarns = do
        caretDiagnostic <-
            if gopt Opt_DiagnosticsShowCaret dflags
            then getCaretDiagnostic severity srcSpan
            else pure empty
        return $ showSDoc dflags $
          withPprStyle pprStyle (message $+$ caretDiagnostic)

      flagMsg =
        case reason of
          NoReason -> Nothing
          Reason wflag -> do
            spec <- flagSpecOf wflag
            return ("-W" ++ flagSpecName spec ++ warnFlagGrp wflag)
          ErrReason Nothing ->
            return "-Werror"
          ErrReason (Just wflag) -> do
            spec <- flagSpecOf wflag
            return $
              "-W" ++ flagSpecName spec ++ warnFlagGrp wflag ++
              ", -Werror=" ++ flagSpecName spec

      warnFlagGrp flag
          | gopt Opt_ShowWarnGroups dflags =
                case smallestGroups flag of
                    [] -> ""
                    groups -> " (in " ++ intercalate ", " (map ("-W"++) groups) ++ ")"
          | otherwise = ""

-- | Find the 'FlagSpec' for a 'WarningFlag'.
flagSpecOf :: WarningFlag -> Maybe (FlagSpec WarningFlag)
flagSpecOf flag = listToMaybe $ filter check wWarningFlags
  where
    check fs = flagSpecFlag fs == flag

-- | Find the smallest group in every hierarchy which a warning
-- belongs to, excluding Weverything.
smallestGroups :: WarningFlag -> [String]
smallestGroups flag = mapMaybe go warningHierarchies where
    -- Because each hierarchy is arranged from smallest to largest,
    -- the first group we find in a hierarchy which contains the flag
    -- is the smallest.
    go (group:rest) = fromMaybe (go rest) $ do
        flags <- lookup group warningGroups
        guard (flag `elem` flags)
        pure (Just group)
    go [] = Nothing
