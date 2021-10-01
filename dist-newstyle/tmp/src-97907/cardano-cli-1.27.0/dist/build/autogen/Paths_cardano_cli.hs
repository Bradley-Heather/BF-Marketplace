{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_cardano_cli (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [1,27,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/bradleyheather/.cabal/store/ghc-8.10.4.20210212/crdn-cl-1.27.0-4adeff6d/bin"
libdir     = "/Users/bradleyheather/.cabal/store/ghc-8.10.4.20210212/crdn-cl-1.27.0-4adeff6d/lib"
dynlibdir  = "/Users/bradleyheather/.cabal/store/ghc-8.10.4.20210212/lib"
datadir    = "/Users/bradleyheather/.cabal/store/ghc-8.10.4.20210212/crdn-cl-1.27.0-4adeff6d/share"
libexecdir = "/Users/bradleyheather/.cabal/store/ghc-8.10.4.20210212/crdn-cl-1.27.0-4adeff6d/libexec"
sysconfdir = "/Users/bradleyheather/.cabal/store/ghc-8.10.4.20210212/crdn-cl-1.27.0-4adeff6d/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cardano_cli_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cardano_cli_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "cardano_cli_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "cardano_cli_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cardano_cli_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cardano_cli_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
