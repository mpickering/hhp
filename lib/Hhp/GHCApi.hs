{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

module Hhp.GHCApi (
    withGHC
  , withGHC'
  , initializeFlagsWithCradle
  , setTargetFiles
  , getDynamicFlags
  , getSystemLibDir
  , withDynFlags
  , withCmdFlags
  , setNoWaringFlags
  , setAllWaringFlags
  ) where

import CoreMonad (liftIO)
import Exception (ghandle, SomeException(..))
import GHC (Ghc, DynFlags(..), GhcLink(..), HscTarget(..), LoadHowMuch(..))
import qualified GHC as G
import qualified Outputable as G

import Control.Monad (forM, void)
import System.Exit (exitSuccess)
import System.IO (hPutStr, hPrint, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)
import System.Directory

import qualified Hhp.Gap as Gap
import Hhp.Types

----------------------------------------------------------------

-- | Obtaining the directory for system libraries.
getSystemLibDir :: IO (Maybe FilePath)
getSystemLibDir = do
    res <- readProcess "ghc" ["--print-libdir"] []
    return $ case res of
        ""   -> Nothing
        dirn -> Just (init dirn)

----------------------------------------------------------------

-- | Converting the 'Ghc' monad to the 'IO' monad.
withGHC :: FilePath  -- ^ A target file displayed in an error message.
        -> Ghc a -- ^ 'Ghc' actions created by the Ghc utilities.
        -> IO a
withGHC file body = ghandle ignore $ withGHC' body
  where
    ignore :: SomeException -> IO a
    ignore e = do
        hPutStr stderr $ file ++ ":0:0:Error:"
        hPrint stderr e
        exitSuccess

withGHC' :: Ghc a -> IO a
withGHC' body = do
    mlibdir <- getSystemLibDir
    G.runGhc mlibdir body

----------------------------------------------------------------

data Build = CabalPkg | SingleFile deriving Eq

-- | Initialize the 'DynFlags' relating to the compilation of a single
-- file or GHC session according to the 'Cradle' and 'Options'
-- provided.
initializeFlagsWithCradle ::
           Options
        -> Cradle
        -> Ghc ()
initializeFlagsWithCradle opt cradle = do
      liftIO $ print "withOptsFile"
      dir <- liftIO $ getCurrentDirectory
      (ex, ghcOpts, err) <- liftIO $ getOptions (cradleOptsProg cradle) (cradleRootDir cradle)
      G.pprTraceM "res" (G.text (show (ex, err, ghcOpts, dir)))
      let compOpts = CompilerOptions (words ghcOpts)
      liftIO $ print ghcOpts
      initSession SingleFile opt compOpts


----------------------------------------------------------------

initSession :: Build
            -> Options
            -> CompilerOptions
            -> Ghc ()
initSession _build Options {..} CompilerOptions {..} = do
    df <- G.getSessionDynFlags
    void $ G.setSessionDynFlags =<< (addCmdOpts ghcOptions
      $ setLinkerOptions
      $ setEmptyLogger df)

setEmptyLogger :: DynFlags -> DynFlags
setEmptyLogger df = df { G.log_action =  \_ _ _ _ _ _ -> return () }

----------------------------------------------------------------

-- we don't want to generate object code so we compile to bytecode
-- (HscInterpreted) which implies LinkInMemory
-- HscInterpreted
setLinkerOptions :: DynFlags -> DynFlags
setLinkerOptions df = df {
    ghcLink   = LinkInMemory
  , hscTarget = HscNothing
  }

addCmdOpts :: [String] -> DynFlags -> Ghc DynFlags
addCmdOpts cmdOpts df1 = do
    (df2, _leftovers, _warns) <- G.parseDynamicFlags df1 (map G.noLoc cmdOpts)
    -- TODO: Need to handle these as well
    -- Ideally it requires refactoring to work in GHCi monad rather than
    -- Ghc monad and then can just use newDynFlags.
    {-
    liftIO $ G.handleFlagWarnings idflags1 warns
    when (not $ null leftovers)
        (throwGhcException . CmdLineError
         $ "Some flags have not been recognized: "
         ++ (concat . intersperse ", " $ map unLoc leftovers))
    when (interactive_only && packageFlagsChanged idflags1 idflags0) $ do
       liftIO $ hPutStrLn stderr "cannot set package flags with :seti; use :set"
    -}
    return df2

----------------------------------------------------------------

-- | Set the files as targets and load them.
setTargetFiles :: [FilePath] -> Ghc ()
setTargetFiles files = do
    targets <- forM files $ \file -> G.guessTarget file Nothing
    G.setTargets (map (\t -> t { G.targetAllowObjCode = False }) targets)
    void $ G.load LoadAllTargets

----------------------------------------------------------------

-- | Return the 'DynFlags' currently in use in the GHC session.
getDynamicFlags :: IO DynFlags
getDynamicFlags = do
    mlibdir <- getSystemLibDir
    G.runGhc mlibdir G.getSessionDynFlags

withDynFlags :: (DynFlags -> DynFlags) -> Ghc a -> Ghc a
withDynFlags setFlag body = G.gbracket setup teardown (\_ -> body)
  where
    setup = do
        dflag <- G.getSessionDynFlags
        void $ G.setSessionDynFlags (setFlag dflag)
        return dflag
    teardown = void . G.setSessionDynFlags

withCmdFlags :: [String] -> Ghc a -> Ghc a
withCmdFlags flags body = G.gbracket setup teardown (\_ -> body)
  where
    setup = do
        dflag <- G.getSessionDynFlags >>= addCmdOpts flags
        void $ G.setSessionDynFlags dflag
        return dflag
    teardown = void . G.setSessionDynFlags

----------------------------------------------------------------

-- | Set 'DynFlags' equivalent to "-w:".
setNoWaringFlags :: DynFlags -> DynFlags
setNoWaringFlags df = df { warningFlags = Gap.emptyWarnFlags}

-- | Set 'DynFlags' equivalent to "-Wall".
setAllWaringFlags :: DynFlags -> DynFlags
setAllWaringFlags df = df { warningFlags = allWarningFlags }

{-# NOINLINE allWarningFlags #-}
allWarningFlags :: Gap.WarnFlags
allWarningFlags = unsafePerformIO $ do
    mlibdir <- getSystemLibDir
    G.runGhc mlibdir $ do
        df <- G.getSessionDynFlags
        df' <- addCmdOpts ["-Wall"] df
        return $ G.warningFlags df'
