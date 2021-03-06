module Paths_recursivo (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/bruno/Documentos/UNESP/LPNC/Aula08-29/recursivo/.stack-work/install/x86_64-linux/lts-6.13/7.10.3/bin"
libdir     = "/home/bruno/Documentos/UNESP/LPNC/Aula08-29/recursivo/.stack-work/install/x86_64-linux/lts-6.13/7.10.3/lib/x86_64-linux-ghc-7.10.3/recursivo-0.1.0.0-1FIyA9JUK0UIwTRGFd6oA6"
datadir    = "/home/bruno/Documentos/UNESP/LPNC/Aula08-29/recursivo/.stack-work/install/x86_64-linux/lts-6.13/7.10.3/share/x86_64-linux-ghc-7.10.3/recursivo-0.1.0.0"
libexecdir = "/home/bruno/Documentos/UNESP/LPNC/Aula08-29/recursivo/.stack-work/install/x86_64-linux/lts-6.13/7.10.3/libexec"
sysconfdir = "/home/bruno/Documentos/UNESP/LPNC/Aula08-29/recursivo/.stack-work/install/x86_64-linux/lts-6.13/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "recursivo_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "recursivo_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "recursivo_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "recursivo_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "recursivo_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
