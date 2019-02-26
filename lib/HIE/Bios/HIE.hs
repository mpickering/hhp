{-# LANGUAGE ScopedTypeVariables #-}
module HIE.Bios.HIE where

import CoreMonad (liftIO)
import CoreUtils (exprType)
import Desugar (deSugarExpr)
import DynFlags (gopt_set, wopt_set, WarningFlag(Opt_WarnTypedHoles))
import GHC
import qualified GHC as G
import qualified Exception as GE
import HscTypes (ModSummary)
import Outputable
import TcType (mkFunTys)

import Control.Applicative ((<|>))
import Control.Monad (filterM)

import HIE.Bios.Doc (getStyle)
import HIE.Bios.GHCApi
import HIE.Bios.Gap
import HIE.Bios.Types
import Debug.Trace
import System.Directory
import EnumSet

-- | Obtaining type of a target expression. (GHCi's type:)
loadFile :: Cradle
      -> Options
      -> FilePath     -- ^ A target file.
      -> IO (G.ParsedModule, TypecheckedModule)
loadFile cradle opt file = withGhcT $ do
  pprTraceM "loadFile:1" (ppr (show cradle, file))
  initializeFlagsWithCradle cradle
  dir <- liftIO $ getCurrentDirectory
  pprTraceM "loadFile:2" (ppr dir)
  liftIO $ setCurrentDirectory "/home/matt/ghc"
  body
  where
    body = inModuleContext file $ \dflag _style -> do
        modSum <- fileModSummary file
        GHC.setSessionDynFlags dflag
        df <- getSessionDynFlags
        pprTraceM "loadFile:3" (ppr $ optLevel df)
        pprTraceM "loadFile:4" (ppr $ show (EnumSet.toList (generalFlags df)))
        p <- G.parseModule modSum
        tcm@TypecheckedModule{tm_typechecked_source = tcs} <- G.typecheckModule p
        return $ (p, tcm)

fileModSummary :: GhcMonad m => FilePath -> m ModSummary
fileModSummary file = do
    mss <- getModSummaries <$> G.getModuleGraph
    let [ms] = filter (\m -> G.ml_hs_file (G.ms_location m) == Just file) mss
    return ms

withContext :: (GhcMonad m) => m a -> m a
withContext action = G.gbracket setup teardown body
  where
    setup = G.getContext
    teardown = setCtx
    body _ = do
        topImports >>= setCtx
        action
    topImports = do
        mss <- getModSummaries <$> G.getModuleGraph
        map modName <$> filterM isTop mss
    isTop mos = lookupMod mos `GE.gcatch` (\(_ :: GE.IOException) -> returnFalse)
    lookupMod mos = G.lookupModule (G.ms_mod_name mos) Nothing >> return True
    returnFalse = return False
    modName = G.IIModule . G.moduleName . G.ms_mod
    setCtx = G.setContext


inModuleContext :: GhcMonad m => FilePath -> (DynFlags -> PprStyle -> m a) -> m a
inModuleContext file action =
    withDynFlags (setWarnTypedHoles . setDeferTypeErrors . setNoWaringFlags) $ do

    df <- getSessionDynFlags
    pprTraceM "loadFile:3" (ppr $ optLevel df)
    pprTraceM "loadFile:4" (text $ show (EnumSet.toList (generalFlags df)))
    setTargetFiles [file]
    withContext $ do
        dflag <- G.getSessionDynFlags
        style <- getStyle dflag
        action dflag style

setDeferTypeErrors :: DynFlags -> DynFlags
setDeferTypeErrors dflag = gopt_set dflag G.Opt_DeferTypeErrors

setWarnTypedHoles :: DynFlags -> DynFlags
setWarnTypedHoles dflag = wopt_set dflag Opt_WarnTypedHoles
