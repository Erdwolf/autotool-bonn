module Paths_autotool_collection (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [1,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/florian/.cabal/bin"
libdir     = "/Users/florian/.cabal/lib/autotool-collection-1.0/ghc-6.12.3"
datadir    = "/Users/florian/.cabal/share/autotool-collection-1.0"
libexecdir = "/Users/florian/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "autotool_collection_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "autotool_collection_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "autotool_collection_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "autotool_collection_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
