{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_automata (
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
bindir     = "/home/felix/Documents/Main/Projects/automata/.stack-work/install/x86_64-linux/c79a3d9f6dc4056351adde105d444416741444c6cfed3b94c9a6099eb94ecd1d/9.4.7/bin"
libdir     = "/home/felix/Documents/Main/Projects/automata/.stack-work/install/x86_64-linux/c79a3d9f6dc4056351adde105d444416741444c6cfed3b94c9a6099eb94ecd1d/9.4.7/lib/x86_64-linux-ghc-9.4.7/automata-0.1.0.0-9HJPpQBvcAc2LNDau2BI37"
dynlibdir  = "/home/felix/Documents/Main/Projects/automata/.stack-work/install/x86_64-linux/c79a3d9f6dc4056351adde105d444416741444c6cfed3b94c9a6099eb94ecd1d/9.4.7/lib/x86_64-linux-ghc-9.4.7"
datadir    = "/home/felix/Documents/Main/Projects/automata/.stack-work/install/x86_64-linux/c79a3d9f6dc4056351adde105d444416741444c6cfed3b94c9a6099eb94ecd1d/9.4.7/share/x86_64-linux-ghc-9.4.7/automata-0.1.0.0"
libexecdir = "/home/felix/Documents/Main/Projects/automata/.stack-work/install/x86_64-linux/c79a3d9f6dc4056351adde105d444416741444c6cfed3b94c9a6099eb94ecd1d/9.4.7/libexec/x86_64-linux-ghc-9.4.7/automata-0.1.0.0"
sysconfdir = "/home/felix/Documents/Main/Projects/automata/.stack-work/install/x86_64-linux/c79a3d9f6dc4056351adde105d444416741444c6cfed3b94c9a6099eb94ecd1d/9.4.7/etc"

getBinDir     = catchIO (getEnv "automata_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "automata_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "automata_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "automata_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "automata_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "automata_sysconfdir") (\_ -> return sysconfdir)




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
