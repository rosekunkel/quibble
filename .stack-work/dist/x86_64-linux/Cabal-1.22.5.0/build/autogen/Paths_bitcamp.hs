module Paths_bitcamp (
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

bindir     = "/srv/bitcamp-2016/.stack-work/install/x86_64-linux/lts-5.11/7.10.3/bin"
libdir     = "/srv/bitcamp-2016/.stack-work/install/x86_64-linux/lts-5.11/7.10.3/lib/x86_64-linux-ghc-7.10.3/bitcamp-0.1.0.0-BFcNWoA2r36BUJU7ciBLw0"
datadir    = "/srv/bitcamp-2016/.stack-work/install/x86_64-linux/lts-5.11/7.10.3/share/x86_64-linux-ghc-7.10.3/bitcamp-0.1.0.0"
libexecdir = "/srv/bitcamp-2016/.stack-work/install/x86_64-linux/lts-5.11/7.10.3/libexec"
sysconfdir = "/srv/bitcamp-2016/.stack-work/install/x86_64-linux/lts-5.11/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "bitcamp_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "bitcamp_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "bitcamp_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bitcamp_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "bitcamp_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
