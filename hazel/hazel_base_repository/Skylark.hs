-- | Datatypes for constructing and rendering BUILD file contents.
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Skylark
    ( Statements
    , Statement(..)
    , Expr(..)
    , expr
    , (=:)
    , renderStatements
    , renderExpr
    ) where

import Text.PrettyPrint
    ( Doc
    , (<>)
    , (<+>)
    )

#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif

import qualified Text.PrettyPrint as Pretty

-- | The top-level contents of a BUILD file.
type Statements = [Statement]

data Statement
    = Rule String [(String, Expr)]
    | Load String [String]
    | Assign String Expr

data Expr
    = ExprInt Int
    | ExprBool Bool
    | ExprNone
    | ExprString String
    | ExprList [Expr]
    | ExprTuple [Expr]
    | ExprDict [(Expr, Expr)]
    | ExprCall String [(String, Expr)]
    | ExprOp Expr String Expr

-- | A helper class for constructing Skylark expressions from Haskell types.
class IsExpr e where
    expr :: e -> Expr

-- | Helper function for assigning the parameter of a rule.
-- For example, @"x" =: [False]@ turns into the Skylark expression
-- @x=[False]@.
(=:) :: IsExpr e => String -> e -> (String, Expr)
s =: x = (s, expr x)
infixr 0 =:

instance {-# OVERLAPPABLE #-} IsExpr e => IsExpr [e] where
    expr = ExprList . map expr

instance IsExpr Int where
    expr = ExprInt

instance IsExpr Bool where
    expr = ExprBool

instance IsExpr String where
    expr = ExprString

instance IsExpr Expr where
    expr = id

instance (IsExpr k, IsExpr v) => IsExpr [(k, v)] where
    expr = ExprDict . map (\(k,v) -> (expr k, expr v))

renderStatements :: [Statement] -> Doc
renderStatements = Pretty.vcat . map renderStatement
  where
    renderStatement (Load e es) = call "load" . map (renderExpr . expr) $ e : es
    renderStatement (Rule s es) = call s $ map (uncurry assign) es
    renderStatement (Assign k e) =
        Pretty.sep
            [ Pretty.text k <+> Pretty.equals <+> Pretty.lparen
            , renderExpr e
            , Pretty.rparen
            ]


assign :: String -> Expr -> Doc
assign k e = Pretty.sep [Pretty.text k <+> Pretty.equals, Pretty.nest 2 $ renderExpr e]

call :: String -> [Doc] -> Doc
call name = wrap (Pretty.text name <> Pretty.lparen) Pretty.rparen

renderExpr :: Expr -> Doc
renderExpr (ExprInt n) = Pretty.int n
renderExpr (ExprBool e) = Pretty.text (show e) -- Haskell Show matches up with Python
renderExpr ExprNone = Pretty.text "None"
renderExpr (ExprString s) = Pretty.text (show s)
renderExpr (ExprList es) = wrap Pretty.lbrack Pretty.rbrack $ map renderExpr es
renderExpr (ExprTuple es) = wrap Pretty.lparen Pretty.rparen $ map renderExpr es
renderExpr (ExprDict es) = wrap Pretty.lbrace Pretty.rbrace $ map entry es
  where
    entry (k, v) = Pretty.sep [renderExpr k <> Pretty.colon, Pretty.nest 2 $ renderExpr v]
renderExpr (ExprCall f args) = call f $ map (uncurry assign) args
renderExpr (ExprOp a op b) = renderExpr a <+> Pretty.text op <+> renderExpr b

wrap :: Doc -> Doc -> [Doc] -> Doc
wrap begin end inner = Pretty.sep
    [begin, Pretty.nest 2 $ Pretty.sep $ map (<> Pretty.comma) inner, end]
