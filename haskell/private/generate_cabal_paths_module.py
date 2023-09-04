""" Generates a Paths_libname module instead of cabal.

We modify the getDataFileName and getDataDir functions to use bazel's runfile library.
And we try to keep the same behavior as before for the others functions
(getBinDir getLibDir getDynLibDir getLibexecDir getSysconfDir).

These functions are defined by cabal through the following files:
https://github.com/haskell/cabal/blob/master/Cabal/src/Distribution/Simple/Build/PathsModule.hs
https://github.com/haskell/cabal/blob/master/templates/Paths_pkg.template.hs

For windows we need to implement the "absolute" case of the template.
Otherwise we want the "relocatable" case since we called configure with "--enable-relocatable".

"""
import re
import os
import json
import ast
from subprocess import (Popen, PIPE)

def normalise_os(os):
    """ recognise os name using aliases known to cabal """
    os = os.lower()
    if os in ["mingw32", "win32", "cygwin32"]: return "windows"
    if os == "darwin": return "osx"
    if os == "gnu": return "hurd"
    if os == "kfreebsdgnu": return "freebsd"
    if os == "solaris2": return "solaris"
    if os in ["linux-android", "linux-androideabi", "linux-androideabihf"]:
        return "android"
    return os

def normalise_arch(arch):
    """ recognise architecture name using aliases known to cabal """
    arch = arch.lower()
    if arch == "powerpc": return "ppc"
    if arch in ["powerpc64", "powerpc64le"]: return "ppc64"
    if arch in ["sparc64", "sun4"]: return "sparc"
    if arch in ["mipsel", "mipseb"]: return "mips"
    if arch in ["armeb", "armel"]: return "arm"
    if arch == "arm64": return "aarch64"
    return arch
 
def generate_cabal_paths_module(component_name, ghc_version, is_windows, cabal_basename, cabal_dirname,
                                ghc, libdir, dynlibdir, bindir, datadir, pkgroot, workspace):

    # cabal calls ghc --info to recover the target arch and os, and uses these in path names.
    # https://github.com/haskell/cabal/blob/496d6fcc26779e754523a6cc7576aea49ef8056e/Cabal/src/Distribution/Simple/GHC/Internal.hs#L87
    # So we do the same.
    p = Popen(f"{ghc} --info", shell=True, stdout=PIPE)
    ghc_info_string = p.stdout.read().decode("utf-8")
    ghc_info = ast.literal_eval("("+ghc_info_string+")")
    for (k, v) in ghc_info:
        if k == "Target platform":
            m = re.match("([^-]*)-[^-]*-([^-]*)", v, re.IGNORECASE)
            if m:
                target_arch = normalise_arch(m.group(1))
                target_os = normalise_os(m.group(2))
                ghc_version_string = ".".join((str(n) for n in ghc_version))
                config = f"{target_arch}-{target_os}-ghc-{ghc_version_string}"

    # Recover the package version and name from the cabal file
    with open(cabal_basename) as cabal_file:
        cabal_file_content = cabal_file.readlines()
    package_version = None
    cabal_package_name = None
    for line in cabal_file_content:
        m = re.match(r"version:\s*([\d.]+)", line, re.IGNORECASE)
        if m :
            package_version = m.group(1)
            continue
        m = re.match(r"name:\s*([a-zA-Z0-9\-]+)", line, re.IGNORECASE)
        if m :
            cabal_package_name = m.group(1)

    if not package_version:
        print("could not find a package version inside cabal file", file=sys.stderr)
        exit(1)

    if not cabal_package_name:
        print("could not find a package name inside cabal file", file=sys.stderr)
        exit(1)

    mangled_cabal_package_name =  cabal_package_name.replace("-", "_")

    supports_cpp = ghc_version >= [6,6,1]
    supports_rebindable_syntax = ghc_version >= [7,0,1]
    package_version_numbers = [int(c) for c in package_version.split('.')]
    version_definition = """version = Version {} []""".format(package_version_numbers)

    if is_windows:
        cabal_dirname = cabal_dirname.replace("/", r"\\")
        path_separator = r'\\'
        is_path_separator_definition = r"""isPathSeparator c = c == '/' || c == '\\'"""
        other_functions = r"""
prefix :: FilePath
prefix = "{pkgroot}"

getBinDir     = return $ prefix `joinFileName` bindir
getLibDir     = return $ prefix `joinFileName` libdir
getDynLibDir  = return $ prefix `joinFileName` dynlibdir
getLibexecDir = return $ prefix `joinFileName` ("{component_name}-{package_version}" ++ libexecCommonSuffix)
getSysconfDir = return $ prefix `joinFileName` sysconfdir
""".format(
    pkgroot = pkgroot.replace("\\", r"\\"),
    component_name = component_name,
    package_version = package_version,
           )
    else:
        path_separator = '/'
        is_path_separator_definition = """isPathSeparator c = c == '/'"""
        other_functions = r"""
getPrefixDirReloc :: FilePath -> IO FilePath
getPrefixDirReloc dirRel = do
    exePath <- getExecutablePath
    let (dir,_) = splitFileName exePath
    return ((dir `minusFileName` bindir) `joinFileName` dirRel)

getExePath = getExecutablePath
getBinDir     = catchIO (getEnv $ packageName++"_bindir")     (\_ -> getPrefixDirReloc bindir)
getLibDir     = catchIO (getEnv $ packageName++"_libdir")     (\_ -> getPrefixDirReloc libdir)
getDynLibDir  = catchIO (getEnv $ packageName++"_dynlibdir")  (\_ -> getPrefixDirReloc dynlibdir)
getLibexecDir = catchIO (getEnv $ packageName++"_libexecdir") (\_ -> getPrefixDirReloc libexecdir)
getSysconfDir = catchIO (getEnv $ packageName++"_sysconfdir") (\_ -> getPrefixDirReloc sysconfdir)

"""

    rebindable_syntax_pragma = ("{-# LANGUAGE NoRebindableSyntax #-}"
                                if supports_rebindable_syntax else "")
    if supports_cpp:
        cpp_pragma = "{-# LANGUAGE CPP #-}"
        catch_io_type = r"""
#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
    """
    else:
        cpp_pragma = ""
        catch_io_type = """
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
    """

    cabal_paths_file_content = r"""
-- Based on the Paths_pkg.template.hs file from cabal.
-- We are only interested in the relocatable case.
{cpp_pragma}
{rebindable_syntax_pragma}

{{-# LANGUAGE ForeignFunctionInterface #-}}
{{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}}
{{-# OPTIONS_GHC -w #-}}

module Paths_{mangled_cabal_package_name} (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Bazel.Runfiles as Runfiles
import System.Environment (getEnv, getExecutablePath)
import qualified Control.Exception as Exception
import Data.Version (Version(..))
import Prelude

{catch_io_type}
catchIO = Exception.catch

version :: Version
{version_definition}

s = [pathSeparator]
dataDirWorkspacePath = "{workspace}"++s++"{cabal_dirname}"++s++"_install"++s++"{datadir}"
packageName = "{component_name}"

libdir = "{libdir}"
bindir = "{bindir}"
dynlibdir = "{dynlibdir}"
libexecCommonSuffix = s ++ "{config}" ++ s ++ "{component_name}-{package_version}"
libexecdir = "libexec" ++ libexecCommonSuffix
sysconfdir = "etc"

getDataDirFromBazel :: IO FilePath
getDataDirFromBazel = do
    r <- Runfiles.create
    let path = Runfiles.rlocation r $ dataDirWorkspacePath
    return path

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
    datadir <- getDataDir
    return $ datadir++s++name

getDataDir = catchIO
    (getEnv (packageName++"_datadir"))
    (\_ -> getDataDirFromBazel) 

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
{other_functions}

-- Utility functions 
minusFileName :: FilePath -> String -> FilePath
minusFileName dir ""     = dir
minusFileName dir "."    = dir
minusFileName dir suffix =
    minusFileName (fst (splitFileName dir)) (fst (splitFileName suffix))

splitFileName :: FilePath -> (String, String)
splitFileName p = (reverse (path2++drive), reverse fname)
  where
    (path,drive) = case p of
      (c:':':p') -> (reverse p',[':',c])
      _          -> (reverse p ,"")
    (fname,path1) = break isPathSeparator path
    path2 = case path1 of
      []                           -> "."
      [_]                          -> path1   -- don't remove the trailing slash if
                                            -- there is only one character
      (c:path') | isPathSeparator c -> path'
      _                             -> path1


joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (last dir) = dir ++ fname
  | otherwise                  = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '{path_separator}'

isPathSeparator :: Char -> Bool
{is_path_separator_definition}
    
    """.format(
        cpp_pragma = cpp_pragma,
        component_name = component_name,
        mangled_cabal_package_name = mangled_cabal_package_name,
        catch_io_type = catch_io_type,
        version_definition = version_definition,
        path_separator = path_separator,
        is_path_separator_definition = is_path_separator_definition,
        package_version = package_version,
        rebindable_syntax_pragma = rebindable_syntax_pragma,
        config = config,
        libdir = libdir,
        dynlibdir = dynlibdir,
        bindir = bindir,
        datadir = datadir,
        cabal_dirname = cabal_dirname,
        workspace = workspace,
        other_functions = other_functions,
    )
    cabal_paths_file_name = f"Paths_{mangled_cabal_package_name}.hs"
    return (cabal_paths_file_name, cabal_paths_file_content)
