-- | Resolve Cabal flags and conditionals for a given package.
module Flatten (flattenToDefaultFlags) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup (Semigroup, sconcat)
import Distribution.Compiler (CompilerFlavor(GHC))
import Distribution.Package (Dependency)
import Distribution.PackageDescription
    ( PackageDescription(..)
    , GenericPackageDescription(..)
    , Condition(..)
    , CondTree(..)
    , ConfVar(..)
    , exeName
    , FlagName
    , flagName
    , flagDefault
    , testName
    )
import Distribution.System (buildOS, buildArch)
import Distribution.Types.CondTree (CondBranch(..))
import Distribution.Version (Version, withinRange)

import qualified Data.Map as Map

-- | Resolve the flags and conditionals for the given package,
-- assuming the given GHC version.
flattenToDefaultFlags
  :: Version
  -> Map.Map FlagName Bool
  -> GenericPackageDescription
  -> PackageDescription
flattenToDefaultFlags ghcVersion flagOverrides gdesc = let
    flags = Map.union flagOverrides $
            Map.fromList [(flagName f, flagDefault f) | f <- genPackageFlags gdesc]
    in (packageDescription gdesc)
          { library = resolve ghcVersion flags <$> condLibrary gdesc
          , executables = map (\(n, e) -> (resolve ghcVersion flags e) { exeName = n })
                            $ condExecutables gdesc
          , testSuites = map (\(n, e) -> (resolve ghcVersion flags e) { testName = n })
                            $ condTestSuites gdesc
          -- TODO: more
          }

resolve ::
       Semigroup a
    => Version
    -> Map.Map FlagName Bool
    -> CondTree ConfVar [Dependency] a
    -> a
resolve ghcVersion flags node
    = sconcat
        $ condTreeData node :|
        [ resolve ghcVersion flags t
        | CondBranch cond ifTrue ifFalse <- condTreeComponents node
        , Just t <- [if isTrue ghcVersion flags cond
                        then Just ifTrue
                        else ifFalse]
        ]

-- | Evaluate a single conditional expression.
isTrue :: Version -> Map.Map FlagName Bool -> Condition ConfVar -> Bool
isTrue ghcVersion flags = loop
  where
    loop (Var (Flag f))
        | Just x <- Map.lookup f flags = x
        | otherwise = error $ "Unknown flag: " ++ show f
    loop (Var (Impl GHC range)) = withinRange ghcVersion range
    loop (Var (Impl _ _)) = False
    loop (Var (OS os)) = os == buildOS
    loop (Var (Arch arch)) = arch == buildArch
    loop (Lit x) = x
    loop (CNot x) = not $ loop x
    loop (COr x y) = loop x || loop y
    loop (CAnd x y) = loop x && loop y


