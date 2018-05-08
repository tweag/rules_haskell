-- Copyright 2018 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--    https://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

-- | This module generates the package_description.bzl file from the .cabal
-- file.  The contents of that file are a straightforward syntactic translation
-- of Cabal's PackageDescription type.
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Google.Google3.Tools.Cabal2Build.Description
    (descriptionFileContents) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Distribution.Compiler (CompilerFlavor)
import Distribution.License (License(..))
import Distribution.ModuleName (ModuleName)
import qualified Distribution.PackageDescription as PackageDescription
import Distribution.PackageDescription
    ( BuildType(..)
    , FlagAssignment
    , FlagName(..)
    , PackageDescription
    , RepoKind(..)
    , RepoType(..)
    , TestSuiteInterface(..)
    )
import Distribution.Package
    ( Dependency(..)
    , PackageName(..)
    , PackageIdentifier(..)
    )
import Distribution.Text (display)
import Distribution.Version (Version(..), VersionRange(..))
import Language.Haskell.Extension (Extension(..), Language(..))
import GHC.Generics
    ( Generic(..)
    , Selector(..)
    , C
    , D
    , K1(..)
    , M1(..)
    , S
    , (:*:)(..)
    )
import Google.Google3.Package (Expr(..), Stmt(..), pPrint)

import Text.PrettyPrint (Doc, ($$), (<+>), (<>), vcat, text)

descriptionFileContents :: FlagAssignment -> PackageDescription -> String
descriptionFileContents flags pkg
    = show $ descriptionFile name flags $ expr pkg
  where PackageName name = pkgName $ PackageDescription.package pkg

descriptionFile :: String -> FlagAssignment -> Expr -> Doc
descriptionFile name flags e =
    "\"\"\"Package description auto-generated from"
        <+> text name <> ".cabal by cabal2build."
    $$ ""
    $$ vcat (map text flagsDesc)
    $$ "\"\"\""
    $$ pPrint (SAssign "description" e)
    $$ ""  -- Ensure newline at end of file
  where
    flagsDesc = case flags of
        [] -> []
        _ -> "Configured with Cabal flags:" : map flagDesc flags
    flagDesc (FlagName flag, value)
        = "  " ++ flag ++ ": " ++ show value

-------------------------------------------------------------------------------

-- | A class for converting Haskell datatypes into Skylark/Python Exprs.
--
-- It uses Generics to help auto-derive the conversion of Haskell records
-- into Skylark "structs".
class Exprable a where
    expr :: a -> Expr

    default expr :: (Generic a, GenExprable (Rep a)) => a -> Expr
    expr = gexpr . from

-- Trivial instances:
instance {-# OVERLAPPING #-} Exprable String where
    expr = LitString . Text.pack

instance Exprable Bool where
    expr = Var . Text.pack . show

instance {-# OVERLAPPABLE #-} Exprable a => Exprable [a] where
    expr = LitList . map expr

instance (Exprable a, Exprable b) => Exprable (a, b) where
    expr (x,y) = LitTuple [expr x, expr y]

instance Exprable a => Exprable (Maybe a) where
    expr Nothing = Var "None"
    expr (Just x) = expr x

instance (Exprable a, Exprable b) => Exprable (Map a b) where
    expr = LitList . map expr . Map.toList

-- | Helper class to get an Expr from the generic representation of a datatype.
class GenExprable f where
    gexpr :: f a -> Expr

instance GenExprable f => GenExprable (M1 D x f) where
    gexpr x = gexpr $ unM1 x

instance Exprable a => GenExprable (K1 i a) where
    gexpr (K1 x) = expr x

-- Haskell records are represented in Generics as nested products (:*:) of
-- fields (M1 S x f) where each field has a selector with a name and a value.
-- For example:
--     data Foo = Foo {foo, bar, baz :: Int}
-- might be represented in Haskell Generics as the nested product
--     (T1 :*: (T2 :*: T3)).
-- The StructFields class lets us collect all those types (T1..T4) into a single
-- list of fields.
class StructFields f where
    structFields :: f a -- ^ Generic representation of a datatype
          -> [(Text.Text, Expr)]  -- ^ Field/value pairs in this datatype.

instance (StructFields f, StructFields g) => StructFields (f :*: g) where
    structFields (f :*: g) = structFields f ++ structFields g

instance (Selector x, GenExprable f) => StructFields (M1 S x f) where
    structFields x = [(Text.pack (selName x), gexpr (unM1 x))]

-- A Haskell record constructor.
instance StructFields f => GenExprable (M1 C x f) where
    gexpr x = struct $ structFields $ unM1 x

-- | A Skylark struct(..) expression.
struct :: [(Text.Text, Expr)] -> Expr
struct = Call "struct" []

stringE :: String -> Expr
stringE = LitString . Text.pack

------

instance Exprable PackageName where
    expr = stringE . display

instance Exprable VersionRange where
    expr = stringE . display

instance Exprable CompilerFlavor where
    expr = stringE . display

instance Exprable Extension where
    expr = stringE . display

instance Exprable Language where
    expr = stringE . display

instance Exprable ModuleName where
    expr = stringE . display

instance Exprable License where
    expr = stringE . display

instance Exprable Version where
    expr = stringE . display

-- Enums that we turn into strings:
instance Exprable RepoType where
    expr (OtherRepoType s) = stringE s
    expr e = stringE $ show e

instance Exprable BuildType where
    expr (UnknownBuildType s) = stringE s
    expr e = stringE $ show e

instance Exprable RepoKind where
    expr (RepoKindUnknown s) = stringE s
    expr e = stringE $ show e

instance Exprable PackageDescription.SetupBuildInfo where
    expr (PackageDescription.SetupBuildInfo setupDepends defaultSetupDepends)
        = struct [("setupDepends", expr setupDepends),
                  ("defaultSetupDepends", expr defaultSetupDepends)]

-- TODO(judahjacobson): Support the benchmark type.  For now, it's not useful.
instance Exprable PackageDescription.BenchmarkInterface where
    expr _ = Var "None"

instance Exprable Dependency where
    expr (Dependency name version)
        = struct [("name", expr name), ("version", expr version)]

instance Exprable TestSuiteInterface where
    expr (TestSuiteExeV10 version path)
        = struct [("type", LitString "exitcode-stdio-1.0"), ("version", expr version), ("mainIs", expr path)]
    -- TODO(judahjacobson): If useful, support other types of tests.
    expr t = stringE $ show t

instance Exprable (PackageDescription.ModuleRenaming) where
    expr m@(PackageDescription.ModuleRenaming flag xs) =
        -- TODO: Move this check out of Exprable
        if flag && null xs
        then expr (flag, xs)
        else error $ "The module-renaming cabal option is not supported yet. " ++ show m

instance Exprable PackageDescription.Benchmark
instance Exprable PackageDescription.BuildInfo
instance Exprable PackageDescription.Library
instance Exprable PackageDescription.ModuleReexport
instance Exprable PackageDescription.PackageDescription
instance Exprable PackageDescription.SourceRepo
instance Exprable PackageDescription.TestSuite
instance Exprable PackageDescription.Executable
instance Exprable PackageIdentifier

-- Currently we ignore "Either"s, since they're not needed for anything useful.
instance Exprable (Either a b) where
    expr _ = Var "None"
