module Paths_benchmarks (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/dghoshal/.cabal/bin"
libdir     = "/home/dghoshal/.cabal/lib/benchmarks-0.1.0.0/ghc-7.0.4"
datadir    = "/home/dghoshal/.cabal/share/benchmarks-0.1.0.0"
libexecdir = "/home/dghoshal/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "benchmarks_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "benchmarks_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "benchmarks_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "benchmarks_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
