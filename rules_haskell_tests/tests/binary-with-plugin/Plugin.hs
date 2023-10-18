{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
module Plugin (plugin) where

import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import GHC.Types.Literal
import GHC.Driver.Plugins
import GHC.Core
import GHC.Core.Opt.Monad
import GHC.Unit.Module.ModGuts
import GHC.Plugins
import System.Process (readProcess)


plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

-- | @install [arg1, arg2]@ invokes @arg1@ and replaces every string
-- literal with arg2.
install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install [arg1, arg2] todo = do
  when ('$' `elem` arg1) $
    error "Make variable not expanded."
  _ <- liftIO $ readProcess arg1 [] ""
  return $ CoreDoPluginPass arg2 (pass (Char8.pack arg2)) : todo

pass :: ByteString -> ModGuts -> CoreM ModGuts
pass bs guts = do
  let binds = map (replaceString bs) (mg_binds guts)
  return guts { mg_binds = binds }

replaceString :: ByteString -> CoreBind -> CoreBind
replaceString bs (NonRec b e) = NonRec b (replaceInExpr bs e)
replaceString bs (Rec bnds) = Rec $ map (\(b, e) -> (b, replaceInExpr bs e)) bnds

replaceInExpr :: ByteString -> CoreExpr -> CoreExpr
replaceInExpr bs = go
  where
    go = \case
      App e0 e1 -> App (go e0) (go e1)
      Lam b e -> Lam b (go e)
      Let bnd e -> Let bnd (go e)
      Case e0 b t alts ->
        let repInAlt (Alt a bs e) = Alt a bs (go e)
            alts' = map repInAlt alts
         in (Case (go e0) b t alts')
      Cast e c -> Cast (go e) c
      Tick t e -> Tick t (go e)
#if __GLASGOW_HASKELL__ >= 808
      Lit LitString{} -> Lit (LitString bs)
#else
      Lit MachStr{} -> Lit (MachStr bs)
#endif
      e -> e
