import Control.Monad (unless)
import qualified GHC.Paths
import System.Directory (doesDirectoryExist, doesFileExist, executable, getPermissions)
import System.FilePath ((</>))

-- This should not depend on @stackage to to be sure that we are testing the
-- local version of `ghc-paths` instead of a Hackage provided one, if at any
-- point @stackage does not use the local version.

main :: IO ()
main = do
  do
    let name = "GHC.Paths.ghc"
    let file = GHC.Paths.ghc
    putStrLn name
    exists <- doesFileExist file
    unless exists $
      fail $ name ++ " does not exist or is not a file: " ++ file
    perms <- getPermissions file
    unless (executable perms) $
      fail $ name ++ " is not executable: " ++ file

  do
    let name = "GHC.Paths.ghc_pkg"
    let file = GHC.Paths.ghc_pkg
    putStrLn name
    exists <- doesFileExist file
    unless exists $
      fail $ name ++ " does not exist or is not a file: " ++ file
    perms <- getPermissions file
    unless (executable perms) $
      fail $ name ++ " is not executable: " ++ file

  do
    let name = "GHC.Paths.libdir"
    let dir = GHC.Paths.libdir
    putStrLn name
    exists <- doesDirectoryExist dir
    unless exists $
      fail $ name ++ " does not exist or is not a directory: " ++ dir
    exists <- doesFileExist (dir </> "settings")
    unless exists $
      fail $ name ++ "/settings does not exist or is not a file: " ++ (dir </> "settings")

  do
    let name = "GHC.Paths.docdir"
    let dir = GHC.Paths.docdir
    putStrLn name
    exists <- doesDirectoryExist dir
    unless exists $
      fail $ name ++ " does not exist or is not a directory: " ++ dir
    exists <- doesFileExist (dir </> "index.html")
    unless exists $
      fail $ name ++ "/index.html does not exist or is not a file: " ++ (dir </> "index.html")
