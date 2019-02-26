module Hhp.HIE where

import CoreMonad (liftIO)
import CoreUtils (exprType)
import Desugar (deSugarExpr)
import DynFlags (gopt_set, wopt_set, WarningFlag(Opt_WarnTypedHoles))
import GHC
import qualified GHC as G
import HscTypes (ModSummary)
import Outputable (PprStyle)
import TcType (mkFunTys)

import Control.Applicative ((<|>))
import Control.Monad (filterM)

import Hhp.Doc (getStyle)
import Hhp.GHCApi
import Hhp.Gap
import Hhp.Types

-- | Obtaining type of a target expression. (GHCi's type:)
loadFile :: Cradle
      -> Options
      -> FilePath     -- ^ A target file.
      -> IO (G.ParsedModule, TypecheckedModule)
loadFile cradle opt file = withGHC' $ do
  initializeFlagsWithCradle opt cradle
  body
  where
    body = inModuleContext file $ \_dflag _style -> do
        modSum <- fileModSummary file
        p <- G.parseModule modSum
        tcm@TypecheckedModule{tm_typechecked_source = tcs} <- G.typecheckModule p
        return $ (p, tcm)

fileModSummary :: FilePath -> Ghc ModSummary
fileModSummary file = do
    mss <- getModSummaries <$> G.getModuleGraph
    let [ms] = filter (\m -> G.ml_hs_file (G.ms_location m) == Just file) mss
    return ms

withContext :: Ghc a -> Ghc a
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
    isTop mos = lookupMod mos <|> returnFalse
    lookupMod mos = G.lookupModule (G.ms_mod_name mos) Nothing >> return True
    returnFalse = return False
    modName = G.IIModule . G.moduleName . G.ms_mod
    setCtx = G.setContext


inModuleContext :: FilePath -> (DynFlags -> PprStyle -> Ghc a) -> Ghc a
inModuleContext file action =
    withDynFlags (setWarnTypedHoles . setDeferTypeErrors . setNoWaringFlags) $ do
    setTargetFiles [file]
    withContext $ do
        dflag <- G.getSessionDynFlags
        style <- getStyle dflag
        action dflag style

setDeferTypeErrors :: DynFlags -> DynFlags
setDeferTypeErrors dflag = gopt_set dflag G.Opt_DeferTypeErrors

setWarnTypedHoles :: DynFlags -> DynFlags
setWarnTypedHoles dflag = wopt_set dflag Opt_WarnTypedHoles
