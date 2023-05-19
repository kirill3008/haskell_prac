{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_prac (
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
bindir     = "/home/arkhip0v0/Documents/heskell/haskell_prac/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/216ffbb784548d4e3be63f95ef62785b11638f684f3f451e3b1169e43ad9e8e2/9.2.7/bin"
libdir     = "/home/arkhip0v0/Documents/heskell/haskell_prac/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/216ffbb784548d4e3be63f95ef62785b11638f684f3f451e3b1169e43ad9e8e2/9.2.7/lib/x86_64-linux-ghc-9.2.7/prac-0.1.0.0-C9vstoNMjo9GWku29VSLXY-prac-exe"
dynlibdir  = "/home/arkhip0v0/Documents/heskell/haskell_prac/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/216ffbb784548d4e3be63f95ef62785b11638f684f3f451e3b1169e43ad9e8e2/9.2.7/lib/x86_64-linux-ghc-9.2.7"
datadir    = "/home/arkhip0v0/Documents/heskell/haskell_prac/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/216ffbb784548d4e3be63f95ef62785b11638f684f3f451e3b1169e43ad9e8e2/9.2.7/share/x86_64-linux-ghc-9.2.7/prac-0.1.0.0"
libexecdir = "/home/arkhip0v0/Documents/heskell/haskell_prac/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/216ffbb784548d4e3be63f95ef62785b11638f684f3f451e3b1169e43ad9e8e2/9.2.7/libexec/x86_64-linux-ghc-9.2.7/prac-0.1.0.0"
sysconfdir = "/home/arkhip0v0/Documents/heskell/haskell_prac/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/216ffbb784548d4e3be63f95ef62785b11638f684f3f451e3b1169e43ad9e8e2/9.2.7/etc"

getBinDir     = catchIO (getEnv "prac_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "prac_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "prac_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "prac_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "prac_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "prac_sysconfdir") (\_ -> return sysconfdir)




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
