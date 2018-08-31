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
--
-- | This module generates the package_description.bzl file from the .cabal
-- file.  The contents of that file are a straightforward syntactic translation
-- of Cabal's PackageDescription type.
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Description (packageDescriptionExpr) where

import Data.Map (Map)
import qualified Data.Map as Map
import Distribution.Compiler (CompilerFlavor)
import Distribution.License (License(..))
import Distribution.ModuleName (ModuleName)
import qualified Distribution.PackageDescription as PackageDescription
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Types.ExecutableScope (ExecutableScope(..))
import Distribution.Types.ExeDependency (ExeDependency(..))
import Distribution.Types.ForeignLib
    ( ForeignLib(..)
    , LibVersionInfo
    , libVersionInfoCRA
    )
import Distribution.Types.ForeignLibOption (ForeignLibOption(..))
import Distribution.Types.ForeignLibType (ForeignLibType(..))
import Distribution.Types.IncludeRenaming (IncludeRenaming(..))
import Distribution.Types.LegacyExeDependency (LegacyExeDependency(..))
import Distribution.Types.Mixin (Mixin(..))
import Distribution.Types.PkgconfigDependency (PkgconfigDependency(..))
import Distribution.Types.PkgconfigName (PkgconfigName)
import Distribution.Types.UnqualComponentName (UnqualComponentName)
#endif
import Distribution.PackageDescription
    ( BuildType(..)
    , PackageDescription
    , RepoKind(..)
    , RepoType(..)
    , TestSuiteInterface(..)
    )
import Distribution.Package
    ( Dependency(..)
    , PackageName
    , PackageIdentifier(..)
    )
import Distribution.Text (display)
import Distribution.Version (Version, VersionRange(..))
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

import Skylark (Expr(..))

-- | A Skylark expression reifying an entire Cabal PackageDescription.
packageDescriptionExpr :: PackageDescription -> Expr
packageDescriptionExpr = expr

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
    expr = ExprString

instance Exprable Bool where
    expr = ExprBool

instance Exprable Int where
    expr = ExprInt

instance {-# OVERLAPPABLE #-} Exprable a => Exprable [a] where
    expr = ExprList . map expr

instance (Exprable a, Exprable b) => Exprable (a, b) where
   expr (x,y) = ExprTuple [expr x, expr y]

instance (Exprable a, Exprable b, Exprable c) => Exprable (a, b, c) where
   expr (x,y,z) = ExprTuple [expr x, expr y, expr z]

instance Exprable a => Exprable (Maybe a) where
    expr Nothing = ExprNone
    expr (Just x) = expr x

instance (Exprable a, Exprable b) => Exprable (Map a b) where
      expr = ExprDict . map (\(x,y) -> (expr x, expr y)) . Map.toList

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
          -> [(String, Expr)]  -- ^ Field/value pairs in this datatype.

instance (StructFields f, StructFields g) => StructFields (f :*: g) where
    structFields (f :*: g) = structFields f ++ structFields g

instance (Selector x, GenExprable f) => StructFields (M1 S x f) where
    structFields x = [(selName x, gexpr (unM1 x))]

-- A Haskell record constructor.
instance StructFields f => GenExprable (M1 C x f) where
    gexpr x = struct $ structFields $ unM1 x

-- | A Skylark struct(..) expression.
struct :: [(String, Expr)] -> Expr
struct = ExprCall "struct"

stringE :: String -> Expr
stringE = ExprString

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
#if MIN_VERSION_Cabal(2,2,0)
    expr e = stringE $ show e
#else
    expr (UnknownBuildType s) = stringE s
    expr e = stringE $ show e
#endif

instance Exprable RepoKind where
    expr (RepoKindUnknown s) = stringE s
    expr e = stringE $ show e

instance Exprable PackageDescription.SetupBuildInfo where
    expr (PackageDescription.SetupBuildInfo setupDepends defaultSetupDepends)
        = struct [("setupDepends", expr setupDepends),
                  ("defaultSetupDepends", expr defaultSetupDepends)]

-- TODO(judahjacobson): Support the benchmark type.  For now, it's not useful.
instance Exprable PackageDescription.BenchmarkInterface where
    expr _ = ExprNone

instance Exprable Dependency where
    expr (Dependency name version)
        = struct [("name", expr name), ("version", expr version)]

instance Exprable TestSuiteInterface where
    expr (TestSuiteExeV10 version path)
        = struct [("type", ExprString "exitcode-stdio-1.0"), ("version", expr version), ("mainIs", expr path)]
    -- TODO(judahjacobson): If useful, support other types of tests.
    expr t = stringE $ show t

instance Exprable (PackageDescription.ModuleRenaming) where
#if MIN_VERSION_Cabal(2,0,0)
    expr m = error $ "The module-renaming cabal option is not supported yet. "
                      ++ show m
#else
    expr m@(PackageDescription.ModuleRenaming flag xs) =
        -- TODO: Move this check out of Exprable
        if flag && null xs
        then expr (flag, xs)
        else error $ "The module-renaming cabal option is not supported yet. " ++ show m
#endif

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
    expr _ = ExprNone

#if MIN_VERSION_Cabal(2,0,0)
instance Exprable UnqualComponentName where
    expr = stringE . display

instance Exprable ExeDependency where
    expr (ExeDependency pkg comp version)
        = struct [ ("packageName", expr pkg)
                 , ("comp", expr comp)
                 , ("version", expr version)
                 ]

instance Exprable LegacyExeDependency where
    expr (LegacyExeDependency name version)
        = struct [("name", expr name), ("version", expr version)]

instance Exprable ExecutableScope where
    expr = stringE . display

instance Exprable ForeignLib

instance Exprable ForeignLibOption where
    expr = stringE . display

instance Exprable ForeignLibType where
    expr = stringE . display

instance Exprable LibVersionInfo where
    expr = expr . libVersionInfoCRA

instance Exprable PkgconfigDependency where
    expr (PkgconfigDependency name version)
        = struct [("name", expr name), ("version", expr version)]

instance Exprable PkgconfigName where
    expr = stringE . display

instance Exprable IncludeRenaming
instance Exprable Mixin
#endif

