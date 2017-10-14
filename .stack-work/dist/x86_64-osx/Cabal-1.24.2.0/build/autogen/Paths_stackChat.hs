{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_stackChat (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/Sam/stackChat/.stack-work/install/x86_64-osx/lts-9.8/8.0.2/bin"
libdir     = "/Users/Sam/stackChat/.stack-work/install/x86_64-osx/lts-9.8/8.0.2/lib/x86_64-osx-ghc-8.0.2/stackChat-0.1.0.0"
dynlibdir  = "/Users/Sam/stackChat/.stack-work/install/x86_64-osx/lts-9.8/8.0.2/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/Sam/stackChat/.stack-work/install/x86_64-osx/lts-9.8/8.0.2/share/x86_64-osx-ghc-8.0.2/stackChat-0.1.0.0"
libexecdir = "/Users/Sam/stackChat/.stack-work/install/x86_64-osx/lts-9.8/8.0.2/libexec"
sysconfdir = "/Users/Sam/stackChat/.stack-work/install/x86_64-osx/lts-9.8/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "stackChat_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "stackChat_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "stackChat_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "stackChat_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "stackChat_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "stackChat_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
