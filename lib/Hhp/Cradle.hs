module Hhp.Cradle (
    findCradle
  ) where

import System.Process
import System.Exit
import Hhp.Types

----------------------------------------------------------------

-- | Finding 'Cradle'.
--   Find a cabal file by tracing ancestor directories.
--   Find a sandbox according to a cabal sandbox config
--   in a cabal directory.
findCradle :: FilePath -> IO Cradle
findCradle wdir = do
    hhpFile wdir

-- | Find a cradle by finding an executable `hhp-opts` file.
hhpFile :: FilePath -> IO Cradle
hhpFile wdir = do
--  (rdir,cfile) <- cabalDir wdir
--  pkgDbStack <- getPackageDbStack rdir
  print "Using opts"
  return Cradle {
      cradleCurrentDir = wdir
    , cradleRootDir    = wdir
    , cradleOptsProg   = CradleAction "demo" mockAction
  }

mockAction :: FilePath -> IO (ExitCode, String, [String])
mockAction _fp = do
  (ex, res, std) <- readProcessWithExitCode "/home/matt/ghc/hpp-wrapper" [] []
  return (ex, std, words res)
