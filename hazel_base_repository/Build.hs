module Build
    ( PackageFiles
    , buildRules
    ) where

import Debug.Trace
import Control.Monad.Trans.Writer.Strict (Writer(..), tell, execWriter)
import Data.List ((\\), intercalate, isPrefixOf)
import Data.Maybe (fromMaybe, maybeToList)
import Distribution.Compiler (CompilerFlavor(GHC))
import Distribution.Simple.Build.Macros (generatePackageVersionMacros)
import Distribution.Text (display)
import Language.Haskell.Extension (Language(Haskell98))
import System.FilePath
    ( (<.>)
    , (</>)
    , dropExtension
    , joinPath
    , normalise
    , splitDirectories
    , takeExtension
    )

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
        ["haskell_library", "haskell_binary"]
    , Load "@ai_formation_hazel//:hazel.bzl"
        ["hazel_paths_module", "hazel_writefile", "hazel_symlink"]
    , Rule "package" ["default_visibility" =: ["//visibility:public"]]
    , Rule "filegroup" ["name" =: "files", "srcs" =: nubOrd allRuleNames]
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
    allRules = execWriter $ do
                  mapM_ (renderLibrary packageFiles prebuilt pkg) (P.library pkg)
                  mapM_ (renderBinary packageFiles prebuilt pkg) (P.executables pkg)
    allRuleNames = [":" ++ n | Rule _ es <- allRules, ("name", ExprString n) <- es]
    allDependencies =
          map P.depPkgName
          . concatMap P.targetBuildDepends
            $ map P.libBuildInfo (maybeToList $ P.library pkg)
            ++ map P.buildInfo (P.executables pkg)
    get m k = case Map.lookup k m of
                Nothing -> error $ "Missing key " ++ show k
                Just x -> x
renderLibrary :: PackageFiles -> PackageList -> P.PackageDescription -> P.Library -> Writer [Statement] ()
renderLibrary packageFiles prebuilt pkg lib
    | null (P.exposedModules lib)
        = tellRule "cc_library" ["name" =: name]
    | otherwise = do
        attrs <- collectHaskellAttrs packageFiles prebuilt pkg
                                      (P.libBuildInfo lib)
                                      name (TargetLibrary $ P.exposedModules lib)
        tellRule "haskell_library"
            $ ["hidden_modules" =: map display (P.otherModules (P.libBuildInfo lib))
              ] ++ attrs
  where
    name = "lib-" ++ display (P.packageName pkg)

renderBinary :: PackageFiles -> PackageList -> P.PackageDescription
    -> P.Executable -> Writer [Statement] ()
renderBinary packageFiles prebuilt pkg exe
    | not (P.buildable $ P.buildInfo exe) = return ()
    | otherwise = do
        attrs <- collectHaskellAttrs packageFiles prebuilt pkg
                                      (P.buildInfo exe)
                                      ("exe-" ++ display (P.exeName exe))
                                      (TargetBinary $ P.modulePath exe)
        tellRule "haskell_binary" attrs

data TargetResult
    = TargetBinary { targetModulePath :: FilePath }
    | TargetLibrary { targetExposedModules :: [ModuleName.ModuleName] }

collectHaskellAttrs ::
       PackageFiles
    -> PackageList
    -> P.PackageDescription
    -> P.BuildInfo
    -> String -- ^ name of thing being generated
    -> TargetResult
    -> Writer [Statement] [(String, Expr)]
collectHaskellAttrs packageFiles prebuilt pkg bi name result = do
    let srcsDir = "srcs-" ++ name
    (srcs, mainFile) <- locateModules
              srcsDir
              (Set.insert ("hazel.paths" </> ModuleName.toFilePath
                                                (pathsModule (P.packageName pkg))
                                <.> "hs")
                  packageFiles)
              bi
              result

    let allDeps = map P.depPkgName $ P.targetBuildDepends bi
    let hazelDeps = nubOrd
                      . map (\d -> "@haskell_" ++ d ++ "//:lib-" ++ d)
                      . map display
                      . filter (`Map.notMember` prebuilt)
                      $ allDeps
    let normalIncludes = nubOrd
                            . filter (`Set.member` packageFiles)
                            $ [ normalise $ d </> f
                              | d <- prepDirs $ P.includeDirs bi
                              , f <- P.includes bi ++ P.installIncludes bi
                              ]

    tellRule "cc_library"
        [ "name" =: "cbits-" ++ name
        , "srcs" =: map normalise $ P.cSources bi
        , "textual_hdrs" =: ExprOp (expr [":cabal_macros"])
                            "+"
                           (glob $ nubOrd $ map normalise (P.extraSrcFiles pkg)
                                              ++ normalIncludes)
        , "includes" =: prepDirs $ P.includeDirs bi
        -- TODO: don't hard-code in "@ghc"
        , "deps" =: ["@ghc//:threaded-rts"]
        , "copts" =: ["-w"]
        ]
    return $
        [ "name" =: name
        , "src_strip_prefix" =: srcsDir
        , "srcs" =: srcs
        , "deps" =: ("cbits-" ++ name) : hazelDeps
        , "prebuilt_dependencies" =: map display $ filter (`Map.member` prebuilt)
                                                      allDeps
        , "compiler_flags" =: collectCompilerFlags bi
        ]
        ++ ["main_file" =: f | Just f <- [mainFile]]

collectCompilerFlags :: P.BuildInfo -> [String]
collectCompilerFlags bi =
    map ("-X" ++)
        (display (fromMaybe Haskell98 $ P.defaultLanguage bi)
            : map display (P.defaultExtensions bi ++ P.oldExtensions bi))
    ++ filterOptions (concat [opts | (GHC,opts) <- P.options bi])
    ++ map ("-optP" ++) (P.cppOptions bi)
                    ++ ["-optP-include", "-optPcabal-macros.h"]
                    ++ ["-optc" ++ opt | opt <- P.ccOptions bi]
                    ++ ["-w"] -- We don't care about warnings
                              -- (TODO: configure this)

filterOptions :: [String] -> [String]
filterOptions = filter (`notElem` badOptions)
  where
    badOptions = ["-O0", "-O", "-O2", "-Wall", "-Werror", "-Wwarn"]

symlinkSrc :: FilePath -> FilePath -> Writer [Statement] ()
symlinkSrc src out = tellRule "hazel_symlink"
    [ "name" =: "gen-" ++ fixName out
    , "src" =: src
    , "out" =: out
    ]
  where
    fixName = concatMap $ \c -> case c of
                      '/' -> "-"
                      '-' -> "--"
                      '.' -> "---"
                      _ -> [c]

locateModule :: FilePath -> PackageFiles -> [FilePath] -> ModuleName.ModuleName
    -> Writer [Statement] (FilePath, FilePath)
locateModule dest fs srcDirs m
    | f:_ <- filter (`Set.member` fs) alternatives
          = do
              let out = dest </> ModuleName.toFilePath m <.> takeExtension f
              symlinkSrc f out
              return (f, out)
    | otherwise = error $ "Couldn't locate module " ++ display m
                            ++ " in source directories " ++ show srcDirs
                            ++ ": " ++ show fs
  where
    ruleName = "gen-" ++ dest ++ "-" ++ display m
    alternatives = [ normalise $ d </> ModuleName.toFilePath m <.> ext
                   | d <- srcDirs
                   , ext <- ["hs", "lhs", "hsc"]
                   ]

locateModules :: FilePath -> PackageFiles -> P.BuildInfo -> TargetResult
    -> Writer [Statement] ([FilePath], Maybe FilePath)
locateModules dest files bi result = do
    let srcDirs = prepDirs (P.hsSourceDirs bi) ++ ["hazel.paths"]
    (srcs,gens) <- fmap unzip
                      $ mapM (locateModule dest files srcDirs)
                      $ nubOrd
                      $ P.otherModules bi ++ case result of
                                              TargetBinary{} -> []
                                              TargetLibrary ms -> ms
    case result of
        TargetLibrary{} -> return (gens, Nothing)
        TargetBinary f
            -- Main file is already in the list of other-modules,
            -- so don't make a duplicate rule:
            | g:_ <- Set.toList (Set.fromList mainFileAlternatives
                                  `Set.intersection`
                                    Set.fromList srcs) -> return (gens, Just g)
            -- Main file is present in one of the source dirs (including ".")
            | g:_ <- mainFileAlternatives -> do
                symlinkSrc g out
                return (out:gens, Just out)
            | otherwise -> error $ "Couldn't locate main file " ++ show f
          where
            out = stripDotDot $ dest </> f
            ruleName = "main-gen-" ++ dest ++ "-" ++ f
            mainFileAlternatives =
                    [ g
                    | d <- "" : srcDirs
                    , let g = resolveDotDot $ normalise $ d </> f
                    , g `Set.member` files
                    ]


filePathToModule :: FilePath -> ModuleName.ModuleName
filePathToModule = ModuleName.fromString . intercalate "." . splitDirectories . dropExtension

resolveDotDot :: FilePath -> FilePath
resolveDotDot = joinPath . resolve . splitDirectories
  where
    resolve (x:"..":xs) = resolve xs
    resolve (x:xs) = x : resolve xs
    resolve [] = []

stripDotDot :: FilePath -> FilePath
stripDotDot = joinPath . filter (/= "..") . splitDirectories

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

tellRule :: String -> [(String, Expr)] -> Writer [Statement] ()
tellRule name attrs = tell [Rule name attrs]
