module Build
    ( PackageFiles
    , buildRules
    ) where

import Control.Monad.Trans.Writer.Strict (Writer(..), tell, runWriter)
import Data.List ((\\), isPrefixOf)
import Data.Maybe (fromMaybe, maybeToList)
import Distribution.Compiler (CompilerFlavor(GHC))
import Distribution.Simple.Build.Macros (generatePackageVersionMacros)
import Distribution.Text (display)
import Language.Haskell.Extension (Language(Haskell98))
import System.FilePath ((<.>), (</>), normalise, takeExtension)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Distribution.ModuleName as ModuleName
import qualified Distribution.Types.ComponentName as P
import qualified Distribution.Package as P
import qualified Distribution.PackageDescription as P
import qualified Distribution.Version as Version
import qualified System.Directory as Directory
import qualified System.Posix.Files as Posix

import Skylark

-- | The set of files in this distribution.
-- Used for resolving the locations of modules and header files.
type PackageFiles = Set.Set FilePath

type PackageList = Map.Map P.PackageName Version.Version

-- | Generates a BUILD file which compiles the given package.
buildRules :: PackageFiles -> PackageList -> PackageList -> P.PackageDescription -> [Statement]
buildRules packageFiles packages prebuilt pkg =
    [ Load "@io_tweag_rules_haskell//haskell:haskell.bzl"
        ["haskell_library"]
    , Load "@ai_formation_hazel//:hazel.bzl"
        ["hazel_paths_module", "hazel_writefile", "hazel_symlink"]
    , Rule "package" ["default_visibility" =: ["//visibility:public"]]
    , Rule "filegroup" ["name" =: "files", "srcs" =: allRuleNames]
    , Rule "hazel_paths_module"
        [ "name" =: display (pathsModule (P.packageName pkg))
        , "version_number" =: Version.versionNumbers (P.packageVersion pkg)
        ]
    , Rule "hazel_writefile"
        [ "name" =: "cabal_macros"
        , "output" =: "cabal-macros.h"
        -- TODO: filter
        , "contents" =: generatePackageVersionMacros
                          . map (\n -> P.PackageIdentifier n (get allAll n))
                          $ allDependencies
        ]
    ]
    ++
    allRules
  where
    allAll = packages `mappend` prebuilt
    allRules = foldMap (renderLibrary packageFiles prebuilt pkg) (P.library pkg)
    allRuleNames = [":" ++ n | Rule _ es <- allRules, ("name", ExprString n) <- es]
    allDependencies =
          map P.depPkgName
          . concatMap P.targetBuildDepends
            $ map P.libBuildInfo (maybeToList $ P.library pkg)
            ++ map P.buildInfo (P.executables pkg)
            ++ map P.testBuildInfo (P.testSuites pkg)
            ++ map P.benchmarkBuildInfo (P.benchmarks pkg)
    get m k = case Map.lookup k m of
                Nothing -> error $ "Missing key " ++ show k
                Just x -> x

locateModule :: FilePath -> PackageFiles -> [FilePath] -> ModuleName.ModuleName
    -> Writer [Statement] FilePath
locateModule dest fs srcDirs m
    | f:_ <- filter (`Set.member` fs) alternatives
          = do
              let out = dest </> ModuleName.toFilePath m <.> takeExtension f
              tell [Rule "hazel_symlink"
                    [ "name" =: ruleName
                    , "src" =: f
                    , "out" =: out
                    ]
                 ]
              return out
    | otherwise = error $ "Couldn't locate module " ++ display m
                            ++ " in source directories " ++ show srcDirs
                            ++ ": " ++ show fs
  where
    ruleName = "gen-" ++ dest ++ "-" ++ display m
    alternatives = [ normalise $ d </> ModuleName.toFilePath m <.> ext
                   | d <- srcDirs
                   , ext <- ["hs", "hsc"]
                   ]

locateModules :: FilePath -> PackageFiles -> P.BuildInfo -> [ModuleName.ModuleName]
    -> ([FilePath], [Statement])
locateModules dest files bi modules =
    runWriter . mapM (locateModule dest files (prepDirs (P.hsSourceDirs bi) ++ ["hazel.paths"]))
              $ modules

renderLibrary :: PackageFiles -> PackageList -> P.PackageDescription -> P.Library -> [Statement]
renderLibrary packageFiles prebuilt pkg lib
    | null (P.exposedModules lib)
        = [Rule "cc_library"
              [ "name" =: "lib-" ++ display (P.packageName pkg)
              ]
          ]
    | otherwise =
    [ Rule "haskell_library"
        [ "name" =: "lib-" ++ display (P.packageName pkg)
        , "srcs" =: srcs
        , "hidden_modules" =: map display $ P.otherModules bi
        , "deps" =: ":cbits-lib" : nubOrd haskellDeps
        , "prebuilt_dependencies" =: map display . filter (`Map.member` prebuilt)
                                      $ allDeps
        , "src_strip_prefix" =: srcsDir
        , "compiler_flags" =: map ("-X" ++)
                    (display (fromMaybe Haskell98 $ P.defaultLanguage bi)
                        : map display (P.defaultExtensions bi ++ P.oldExtensions bi))
                    ++ filterOptions (concat [opts | (GHC,opts) <- P.options bi])
                    ++ map ("-optP" ++) (P.cppOptions bi)
                    ++ ["-optP-include", "-optPcabal-macros.h"]
                    ++ ["-optc" ++ opt | opt <- P.ccOptions bi]
                    ++ ["-w"] -- We don't care about warnings
                              -- (TODO: configure this)
        ]
    , Rule "cc_library"
        [ "name" =: "cbits-lib"
        , "srcs" =: P.cSources bi
        , "textual_hdrs" =: let normalIncludes
                                  = nubOrd
                                      . filter (`Set.member` packageFiles)
                                      $ [ d </> f
                                        | d <- prepDirs $ P.includeDirs bi
                                        , f <- P.includes bi ++ P.installIncludes bi
                                        ]
                            in ExprOp (expr $ normalIncludes ++ [":cabal_macros"])
                              "+"
                             (glob $ nubOrd $ P.extraSrcFiles pkg \\ normalIncludes)
        , "includes" =: prepDirs $ P.includeDirs bi
        -- TODO: don't hard-code in "@ghc"
        , "deps" =: ["@ghc//:threaded-rts"]
        ]
    ] ++ srcRules
  where
    pathsMod = pathsModule (P.packageName pkg)
    bi = P.libBuildInfo lib
    allDeps = map P.depPkgName $ P.targetBuildDepends bi
    haskellDeps =  map (\d -> "@haskell_" ++ d ++ "//:lib-" ++ d)
                      . map display
                      . filter (`Map.notMember` prebuilt)
                      $ allDeps
    srcsDir = ".hazel-lib"
    (srcs, srcRules) = locateModules srcsDir
                              (Set.insert ("hazel.paths" </> ModuleName.toFilePath pathsMod
                                              <.> "hs")
                                  packageFiles) bi
                          $ P.exposedModules lib ++ P.otherModules bi
    normalIncludes
          = nubOrd
              . filter (`Set.member` packageFiles)
              $ [ d </> f
                | d <- prepDirs $ P.includeDirs bi
                , f <- P.includes bi ++ P.installIncludes bi
                ]


filterOptions :: [String] -> [String]
filterOptions = filter (`notElem` badOptions)
  where
    badOptions = ["-O0", "-O", "-O2", "-Wall", "-Werror", "-Wwarn"]

glob :: [String] -> Expr
glob fs = ExprCall "glob" ["include" =: fs]

prepDirs :: [FilePath] -> [FilePath]
prepDirs [] = [""]
prepDirs fs = fs

pathsModule :: P.PackageName -> ModuleName.ModuleName
pathsModule n = ModuleName.fromString $ "Paths_" ++ map fixHyphen (display n)
  where
    fixHyphen '-' = '_'
    fixHyphen c = c

nubOrd :: Ord a => [a] -> [a]
nubOrd = Set.toList . Set.fromList
