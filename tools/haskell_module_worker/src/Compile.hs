{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

-- | This module implements a function to compile modules individually
-- in a single GHC session, which should avoid the cost of starting
-- the compiler multiple times.
module Compile (CompileM, Status(..), compile, runSession) where

import Control.Exception (throwIO)
import Control.Monad (guard, forM_, when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Trans.Except ( ExceptT(..), runExceptT )
import Data.Binary (Binary)
import Data.IORef
import Data.List (intercalate, isPrefixOf, partition)
import Data.Maybe
import Data.Time (UTCTime)
import System.FilePath
import System.IO (hFlush, hPutStrLn, stderr)

import GHC hiding (Succeeded)
import GHC.Generics (Generic)
import GHC.Paths (libdir)
import GhcMake (IsBoot(..), findExtraSigImports, implicitRequirements)
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
  , isObjectTarget
  , warningGroups
  , warningHierarchies
  , wWarningFlags
  )
import DriverPhases
import DriverPipeline (compileOne, preprocess, writeInterfaceOnlyMode)
import ErrUtils
  ( ErrorMessages
  , MsgDoc
  , getCaretDiagnostic
  , mkLocMessageAnn
  , pprErrMsgBagWithLoc
  )
import FastString (FastString)
import Finder (addHomeModuleToFinder, mkHomeModLocation)
import HeaderInfo (getImports)
import HscTypes
  ( SourceModified(SourceModified)
  , handleFlagWarnings
  , hsc_dflags
  , srcErrorMessages
  , throwErrors
  )
import Panic (fromException, try)
import PlainPanic (PlainGhcException(PlainPanic))
import Outputable
import StringBuffer (StringBuffer, hGetStringBuffer)
import Util
import System.Process (callCommand)

data Status
  = NonHaskellInputs [String]        -- Can only compile Haskell modules
  | NonOneShotCompilation            -- Can only compile in OneShot mode
  | CompileErrors [String] [String]  -- Logs and compilation errors
  | Succeeded [String]               -- Logs (include warnings)
  deriving (Generic, Show)

instance Binary Status

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
            when (workerVerbosity > 0) $ do
              hPutStrLn stderr msg
              hFlush stderr
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
      let replaceSlash '/' = '.'
          replaceSlash x = x
      liftIO $ callCommand $ "(/usr/bin/pwd; /usr/bin/find -L .) > /tmp/persistent-wfile-" ++ (map replaceSlash $ fst $ head hs_srcs)
      hsc_env <- GHC.getSession
      collectStatus logsRef $
        forM_ hs_srcs $ \(src, phase) -> do
          emodSum <- summariseFile hsc_env src phase True Nothing
          case emodSum of
            Right modSum ->
              compileOne hsc_env modSum 0 0 Nothing Nothing SourceModified
            Left errs -> throwErrors errs
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


-- Copied from
-- https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-8.10.7-release/compiler/main/GhcMake.hs#L2413
summariseFile
        :: HscEnv
        -> FilePath                     -- source file name
        -> Maybe Phase                  -- start phase
        -> Bool                         -- object code allowed?
        -> Maybe (StringBuffer,UTCTime)
        -> IO (Either ErrorMessages ModSummary)

summariseFile hsc_env src_fn0 mb_phase obj_allowed maybe_buf
   = do src_timestamp <- get_src_timestamp
        new_summary src_fn0 src_timestamp
  where
    get_src_timestamp = case maybe_buf of
                           Just (_,t) -> return t
                           Nothing    -> liftIO $ getModificationUTCTime src_fn0
                        -- getModificationUTCTime may fail

    new_summary src_fn src_timestamp = runExceptT $ do
        preimps@PreprocessedImports {..}
            <- getPreprocessedImports hsc_env src_fn mb_phase maybe_buf


        -- Make a ModLocation for this file
        location <- liftIO $ mkHomeModLocation (hsc_dflags hsc_env) pi_mod_name src_fn

        -- Tell the Finder cache where it is, so that subsequent calls
        -- to findModule will find it, even if it's not on any search path
        mod0 <- liftIO $ addHomeModuleToFinder hsc_env pi_mod_name location

        liftIO $ makeNewModSummary hsc_env $ MakeNewModSummary
            { nms_src_fn = src_fn
            , nms_src_timestamp = src_timestamp
            , nms_is_boot = NotBoot
            , nms_hsc_src =
                if isHaskellSigFilename src_fn
                   then HsigFile
                   else HsSrcFile
            , nms_location = location
            , nms_mod = mod0
            , nms_obj_allowed = obj_allowed
            , nms_preimps = preimps
            }

data MakeNewModSummary
  = MakeNewModSummary
      { nms_src_fn :: FilePath
      , nms_src_timestamp :: UTCTime
      , nms_is_boot :: IsBoot
      , nms_hsc_src :: HscSource
      , nms_location :: ModLocation
      , nms_mod :: Module
      , nms_obj_allowed :: Bool
      , nms_preimps :: PreprocessedImports
      }

makeNewModSummary :: HscEnv -> MakeNewModSummary -> IO ModSummary
makeNewModSummary hsc_env MakeNewModSummary{..} = do
  let PreprocessedImports{..} = nms_preimps
  let dflags = hsc_dflags hsc_env

  -- when the user asks to load a source file by name, we only
  -- use an object file if -fobject-code is on.  See #1205.
  obj_timestamp <- liftIO $
      if isObjectTarget (hscTarget dflags)
         || nms_obj_allowed -- bug #1205
          then getObjTimestamp nms_location nms_is_boot
          else return Nothing

  hi_timestamp <- maybeGetIfaceDate dflags nms_location
  hie_timestamp <- modificationTimeIfExists (ml_hie_file nms_location)

  extra_sig_imports <- findExtraSigImports hsc_env nms_hsc_src pi_mod_name
  required_by_imports <- implicitRequirements hsc_env pi_theimps

  return $ ModSummary
      { ms_mod = nms_mod
      , ms_hsc_src = nms_hsc_src
      , ms_location = nms_location
      , ms_hspp_file = pi_hspp_fn
      , ms_hspp_opts = pi_local_dflags
      , ms_hspp_buf  = Just pi_hspp_buf
      , ms_parsed_mod = Nothing
      , ms_srcimps = pi_srcimps
      , ms_textual_imps =
          pi_theimps ++ extra_sig_imports ++ required_by_imports
      , ms_hs_date = nms_src_timestamp
      , ms_iface_date = hi_timestamp
      , ms_hie_date = hie_timestamp
      , ms_obj_date = obj_timestamp
      }

data PreprocessedImports
  = PreprocessedImports
      { pi_local_dflags :: DynFlags
      , pi_srcimps  :: [(Maybe FastString, Located ModuleName)]
      , pi_theimps  :: [(Maybe FastString, Located ModuleName)]
      , pi_hspp_fn  :: FilePath
      , pi_hspp_buf :: StringBuffer
      , pi_mod_name_loc :: SrcSpan
      , pi_mod_name :: ModuleName
      }

-- Preprocess the source file and get its imports
-- The pi_local_dflags contains the OPTIONS pragmas
getPreprocessedImports
    :: HscEnv
    -> FilePath
    -> Maybe Phase
    -> Maybe (StringBuffer, UTCTime)
    -- ^ optional source code buffer and modification time
    -> ExceptT ErrorMessages IO PreprocessedImports
getPreprocessedImports hsc_env src_fn mb_phase maybe_buf = do
  (pi_local_dflags, pi_hspp_fn)
      <- ExceptT $ preprocess hsc_env src_fn (fst <$> maybe_buf) mb_phase
  pi_hspp_buf <- liftIO $ hGetStringBuffer pi_hspp_fn
  (pi_srcimps, pi_theimps, L pi_mod_name_loc pi_mod_name)
      <- ExceptT $ getImports pi_local_dflags pi_hspp_buf pi_hspp_fn src_fn
  return PreprocessedImports {..}

maybeGetIfaceDate :: DynFlags -> ModLocation -> IO (Maybe UTCTime)
maybeGetIfaceDate dflags location
 | writeInterfaceOnlyMode dflags
    -- Minor optimization: it should be harmless to check the hi file location
    -- always, but it's better to avoid hitting the filesystem if possible.
    = modificationTimeIfExists (ml_hi_file location)
 | otherwise
    = return Nothing

getObjTimestamp :: ModLocation -> IsBoot -> IO (Maybe UTCTime)
getObjTimestamp location is_boot
  = if is_boot == IsBoot then return Nothing
                         else modificationTimeIfExists (ml_obj_file location)
