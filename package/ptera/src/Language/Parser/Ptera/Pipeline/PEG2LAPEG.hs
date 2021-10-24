module Language.Parser.Ptera.Pipeline.PEG2LAPEG where

import Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Machine.PEG as PEG
import qualified Language.Parser.Ptera.Machine.LAPEG as LAPEG
import qualified Language.Parser.Ptera.Machine.LAPEG.Builder as LAPEGBuilder
import qualified Language.Parser.Ptera.Machine.LAPEG.LAPEBuilder as LAPEBuilder
import qualified Language.Parser.Ptera.Data.Alignable.Map   as AlignableMap
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Alignable.Set as AlignableSet
import qualified Language.Parser.Ptera.Data.Symbolic.IntSet as SymbolicIntSet
import qualified Data.EnumMap.Strict                        as EnumMap


peg2LaPeg :: PEG.T a -> Except (AlignableSet.T PEG.Var) (LAPEG.T a)
peg2LaPeg g = LAPEGBuilder.build do
    initialCtxBuilder <- get
    let initialCtx = Context
            {
                ctxBuilder = initialCtxBuilder,
                ctxVarMap = AlignableMap.empty,
                ctxAvailables = AlignableMap.empty,
                ctxUpdates = AlignableSet.empty,
                ctxUpdateVarStack = [],
                ctxOriginalRules = PEG.rules g
            }
    let (mx, finalCtx) = runState
            do runExceptT do pipeline do PEG.initials g
            do initialCtx
    case mx of
        Left vs -> lift do throwE vs
        Right{} -> put do ctxBuilder finalCtx
    where
        pipeline inits = do
            rvs <- foldlM
                do \vs1 (s, v) -> catchE
                    do
                        pegInitialPipeline s v
                        pure vs1
                    \vs2 ->
                        pure do AlignableSet.union vs1 vs2
                do AlignableSet.empty
                do EnumMap.assocs inits
            if AlignableSet.null rvs
                then pure ()
                else throwE rvs


type Pipeline a = ExceptT (AlignableSet.T PEG.Var) (State (Context a))

data Context a = Context
    {
        ctxBuilder :: LAPEGBuilder.Context a,
        ctxVarMap :: AlignableMap.T PEG.Var LAPEG.Var,
        ctxAvailables :: AlignableMap.T LAPEG.Var SymbolicIntSet.T,
        ctxUpdates :: AlignableSet.T LAPEG.Var,
        ctxUpdateVarStack :: [PEG.Var],
        ctxOriginalRules :: AlignableArray.T PEG.Var (PEG.PE a)
    }

pegInitialPipeline :: PEG.StartPoint -> PEG.Var -> Pipeline a ()
pegInitialPipeline s v = do
    newV <- getAvailableVar v >>= \case
        Just x ->
            pure x
        Nothing -> do
            (x, _) <- pegVarUpdatePipeline v
            pure x
    liftBuilder do LAPEGBuilder.registerInitial s newV

pegVarUpdatePipeline :: PEG.Var -> Pipeline a (LAPEG.Var, SymbolicIntSet.T)
pegVarUpdatePipeline v = do
    ctx <- lift get
    let pe = AlignableArray.index
            do ctxOriginalRules ctx
            do v
    lift do put do ctx { ctxUpdates = AlignableSet.empty }
    r <- pegRulePipeline v pe
    pegVarStackPipeline
    pure r

pegVarStackPipeline :: Pipeline a ()
pegVarStackPipeline = do
    ctx <- lift get
    goVars ctx do ctxUpdateVarStack ctx
    where
        goVars ctx = \case
            [] ->
                pure ()
            v:vs -> getAvailableVar v >>= \case
                Just _ ->
                    goVars ctx vs
                Nothing -> do
                    lift do put do ctx { ctxUpdateVarStack = vs }
                    _ <- pegVarUpdatePipeline v
                    pegVarStackPipeline

pegRulePipeline :: PEG.Var -> PEG.PE a -> Pipeline a (LAPEG.Var, SymbolicIntSet.T)
pegRulePipeline v (PEG.PE alts) = do
    newV <- getNewVar v
    lift do
        modify' \ctx -> ctx
            {
                ctxUpdates = AlignableSet.insert newV
                    do ctxUpdates ctx
            }
    newPe <- LAPEBuilder.build do
        forM_ alts \alt -> do
            (newAlt, is) <- lift do pegAltPipeline alt
            LAPEBuilder.addAlt is newAlt
    liftBuilder do LAPEGBuilder.addRule newV newPe
    lift do
        modify' \ctx -> ctx
            { ctxAvailables = AlignableMap.insert newV
                do LAPEG.available newPe
                do ctxAvailables ctx
            , ctxUpdates = AlignableSet.delete newV
                do ctxUpdates ctx
            }
    pure (newV, LAPEG.available newPe)

pegAltPipeline :: PEG.Alt a -> Pipeline a (LAPEG.Alt a, SymbolicIntSet.T)
pegAltPipeline alt = case PEG.altUnitSeq alt of
    [] ->
        pure (newAlt [], SymbolicIntSet.full)
    u0:us -> do
        (newU0, is) <- goUnit0 u0
        newUs <- forM us \u -> goUnit u
        pure (newAlt do newU0:newUs, is)
    where
        newAlt us = LAPEG.Alt
            {
                altUnitSeq = us,
                altKind = PEG.altKind alt,
                altAction = PEG.altAction alt
            }

        goUnit0 = \case
            PEG.UnitTerminal t ->
                pure (LAPEG.UnitTerminal t, SymbolicIntSet.singleton t)
            PEG.UnitNonTerminal v -> do
                ctx <- lift get
                case AlignableMap.lookup v do ctxVarMap ctx of
                    Nothing -> do
                        goVarUpdate v
                    Just newV | AlignableSet.member newV do ctxUpdates ctx ->
                        throwV v
                    Just newV -> case AlignableMap.lookup newV do ctxAvailables ctx of
                        Just is ->
                            pure (LAPEG.UnitNonTerminal newV, is)
                        Nothing ->
                            goVarUpdate v

        goVarUpdate v = do
            (newV, is) <- pegVarUpdatePipeline v
            pure (LAPEG.UnitNonTerminal newV, is)

        goUnit = \case
            PEG.UnitTerminal t ->
                pure do LAPEG.UnitTerminal t
            PEG.UnitNonTerminal v -> do
                pushUpdateVar v
                newV <- getNewVar v
                pure do LAPEG.UnitNonTerminal newV

getNewVar :: PEG.Var -> Pipeline a LAPEG.Var
getNewVar v = do
    ctx <- lift get
    case AlignableMap.lookup v do ctxVarMap ctx of
        Just newV ->
            pure newV
        Nothing -> do
            newV <- liftBuilder do LAPEGBuilder.genNewVar
            lift do
                put do
                    ctx { ctxVarMap = AlignableMap.insert v newV do ctxVarMap ctx }
            pure newV

getAvailableVar :: PEG.Var -> Pipeline a (Maybe LAPEG.Var)
getAvailableVar v = do
    ctx <- lift get
    case AlignableMap.lookup v do ctxVarMap ctx of
        Nothing ->
            pure Nothing
        Just newV -> case AlignableMap.lookup newV do ctxAvailables ctx of
            Nothing ->
                pure Nothing
            Just _ ->
                pure do Just newV

pushUpdateVar :: PEG.Var -> Pipeline a ()
pushUpdateVar v = do
    ctx <- lift get
    getAvailableVar v >>= \case
        Just _ ->
            pure ()
        Nothing ->
            lift do put do ctx { ctxUpdateVarStack = v:ctxUpdateVarStack ctx }

throwV :: PEG.Var -> Pipeline a r
throwV v = throwE do AlignableSet.singleton v

liftBuilder :: LAPEGBuilder.T a Identity r -> Pipeline a r
liftBuilder builder = do
    ctx <- lift get
    let (x, builderCtx) = runState builder do ctxBuilder ctx
    lift do put do ctx { ctxBuilder = builderCtx }
    pure x
