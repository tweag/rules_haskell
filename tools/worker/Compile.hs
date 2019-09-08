{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE CPP #-}

module Compile (compile) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.List
import System.FilePath
import System.Exit

import GHC
import GHC.Paths ( libdir )
import DynFlags ( defaultFatalMessager, defaultFlushOut, Option(..) )
import DriverPhases
import DriverPipeline ( compileFile, oneShot )
import Util

compile :: [String] -> IO ()
compile flags =
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just libdir) $ do

        -- Parse flags
        dflags <- getSessionDynFlags
        (dflags2, fileish_args, _warns) <-
          parseDynamicFlags dflags (map noLoc flags)

        -- Normilize paths
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

        -- Update flags with normalized
          dflags3 = dflags2 { ldInputs = map (FileOption "") objs
                                         ++ ldInputs dflags2 }

        _ <- setSessionDynFlags dflags3

        doMake srcs

doMake :: [(String,Maybe Phase)] -> Ghc ()
doMake srcs  = do
    let (hs_srcs, non_hs_srcs) = partition isHaskellishTarget srcs

    hsc_env <- GHC.getSession

    -- if we have no haskell sources from which to do a dependency
    -- analysis, then just do one-shot compilation and/or linking.
    -- This means that "ghc Foo.o Bar.o -o baz" links the program as
    -- we expect.
    if (null hs_srcs)
       then liftIO (oneShot hsc_env StopLn srcs)
       else do

    o_files <- mapM (\x -> liftIO $ compileFile hsc_env StopLn x)
                 non_hs_srcs
    dflags <- GHC.getSessionDynFlags
    let dflags' = dflags { ldInputs = map (FileOption "") o_files
                                      ++ ldInputs dflags }
    _ <- GHC.setSessionDynFlags dflags'

    targets <- mapM (uncurry GHC.guessTarget) hs_srcs
    GHC.setTargets targets
    ok_flag <- GHC.load LoadAllTargets

    when (failed ok_flag) (liftIO $ exitWith (ExitFailure 1))

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
