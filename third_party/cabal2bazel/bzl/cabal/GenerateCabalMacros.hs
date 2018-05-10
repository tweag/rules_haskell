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

-- | A program to output a header file defining macros for a Cabal package.
-- In particular, it defines the MIN_VERSION_{name} macro which many packages
-- use to tell the version of their dependencies.
--
-- For more information on Cabal macros, see:
-- https://www.haskell.org/cabal/users-guide/developing-packages.html#conditional-compilation
--
-- Example Usage:
-- ...../GenerateCabalMacros third_party/haskell/ghc/.../default-packages.txt
--      base
--      bytestring
--      split-0.2.2
--      text-1.2.0.4
--
-- Package names which appear in default-packages.txt (e.g. "base",
-- "bytestring") will be assigned the version number that appears in that file.
-- Other packages (e.g. "split", "text") must have their version given on the
-- command line.
module Main where

import Control.Monad (unless)
import Distribution.Compat.ReadP (look, pfail, readP_to_S, ReadP)
import Distribution.Package (PackageIdentifier(..))
import Distribution.Text (parse)
import Distribution.Simple.Build.Macros (generatePackageVersionMacros)
import System.Environment (getArgs)

main :: IO ()
main = do
    packageStrings <- getArgs
    putStrLn $ generatePackageVersionMacros $ map parsePackage packageStrings

-- | Parse a package id like "base-4.8.2.0" or
-- "base-4.8.2.0-0d6d1084fbc041e1cded9228e80e264d"
-- into a PackageIdentifier (pair of name "base" and version [4,8,2,0]).
parsePackage :: String -> PackageIdentifier
parsePackage s = case readP_to_S (parse <* eof) s of
    [(p,"")] -> p
    cs -> error $ "Parsing " ++ show s ++ " got " ++ show cs

-- | Fail unless this happens at the end of the input.  Fixes ambiguity errors
-- with Cabal's implementations of parse; for example, "text" and "text-1.2.3"
-- are both valid PackageIdentifiers, so we need 'eof' to force the parser to
-- choose the latter.
eof :: ReadP r ()
eof = do
    rest <- look
    unless (null rest) pfail
