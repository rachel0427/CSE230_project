{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_survival_game (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/workspaces/CSE230_project/.stack-work/install/x86_64-linux/5f363aa131f44346864e29e5c955ee96910ac4282012611741f3b9202d1e465f/9.2.7/bin"
libdir     = "/workspaces/CSE230_project/.stack-work/install/x86_64-linux/5f363aa131f44346864e29e5c955ee96910ac4282012611741f3b9202d1e465f/9.2.7/lib/x86_64-linux-ghc-9.2.7/survival-game-0.1.0.0-5JADvT5hiC6JOUw8umc3xV-survival-game"
dynlibdir  = "/workspaces/CSE230_project/.stack-work/install/x86_64-linux/5f363aa131f44346864e29e5c955ee96910ac4282012611741f3b9202d1e465f/9.2.7/lib/x86_64-linux-ghc-9.2.7"
datadir    = "/workspaces/CSE230_project/.stack-work/install/x86_64-linux/5f363aa131f44346864e29e5c955ee96910ac4282012611741f3b9202d1e465f/9.2.7/share/x86_64-linux-ghc-9.2.7/survival-game-0.1.0.0"
libexecdir = "/workspaces/CSE230_project/.stack-work/install/x86_64-linux/5f363aa131f44346864e29e5c955ee96910ac4282012611741f3b9202d1e465f/9.2.7/libexec/x86_64-linux-ghc-9.2.7/survival-game-0.1.0.0"
sysconfdir = "/workspaces/CSE230_project/.stack-work/install/x86_64-linux/5f363aa131f44346864e29e5c955ee96910ac4282012611741f3b9202d1e465f/9.2.7/etc"

getBinDir     = catchIO (getEnv "survival_game_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "survival_game_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "survival_game_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "survival_game_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "survival_game_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "survival_game_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
