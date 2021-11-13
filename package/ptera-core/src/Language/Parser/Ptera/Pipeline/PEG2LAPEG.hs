module Language.Parser.Ptera.Pipeline.PEG2LAPEG where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                             as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable.Array      as AlignableArray
import qualified Language.Parser.Ptera.Data.Alignable.Map        as AlignableMap
import qualified Language.Parser.Ptera.Data.Alignable.Set        as AlignableSet
import qualified Language.Parser.Ptera.Data.Symbolic.IntSet      as SymbolicIntSet
import qualified Language.Parser.Ptera.Machine.LAPEG             as LAPEG
import qualified Language.Parser.Ptera.Machine.LAPEG.Builder     as LAPEGBuilder
import qualified Language.Parser.Ptera.Machine.LAPEG.RuleBuilder as LARuleBuilder
import qualified Language.Parser.Ptera.Machine.PEG               as PEG


peg2LaPeg :: Enum s => PEG.T s a -> Except (AlignableSet.T PEG.Var) (LAPEG.T s a)
peg2LaPeg g = LAPEGBuilder.build do
    initialCtxBuilder <- get
    let initialCtx = Context
            {
                ctxBuilder = initialCtxBuilder,
                ctxVarMap = AlignableMap.empty,
                ctxAvailables = AlignableMap.empty,
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
                        pipelineStep s v
                        pure vs1
                    \vs2 ->
                        pure do AlignableSet.union vs1 vs2
                do AlignableSet.empty
                do EnumMap.assocs inits
            if AlignableSet.null rvs
                then pure ()
                else throwE rvs

        pipelineStep s v = do
            lift do modify' \ctx -> ctx { ctxAvailables = AlignableMap.empty }
            pegInitialPipeline s v
            pegVarStackPipeline


type Pipeline s a = ExceptT (AlignableSet.T PEG.Var) (State (Context s a))

data Context s a = Context
    {
        ctxBuilder        :: LAPEGBuilder.Context s a,
        ctxVarMap         :: AlignableMap.T PEG.Var LAPEG.Var,
        ctxAvailables     :: AlignableMap.T LAPEG.Var (Maybe SymbolicIntSet.T),
        ctxUpdateVarStack :: [PEG.Var],
        ctxOriginalRules  :: AlignableArray.T PEG.Var (PEG.Rule a)
    }

pegInitialPipeline :: Enum s => s -> PEG.Var -> Pipeline s a ()
pegInitialPipeline s v = do
    newV <- getAvailableVar v >>= \case
        Just x ->
            pure x
        Nothing -> do
            (x, _) <- pegVarPipeline v
            pure x
    liftBuilder do LAPEGBuilder.registerInitial s newV

pegVarPipeline :: PEG.Var -> Pipeline s a (LAPEG.Var, SymbolicIntSet.T)
pegVarPipeline v = do
    ctx <- lift get
    case AlignableMap.lookup v do ctxVarMap ctx of
        Nothing -> do
            goVarUpdate
        Just newV -> case AlignableMap.lookup newV do ctxAvailables ctx of
            Nothing ->
                goVarUpdate
            Just Nothing ->
                throwV v
            Just (Just is) ->
                pure (newV, is)
    where
        goVarUpdate = do
            ctx <- lift get
            let pe = AlignableArray.forceIndex
                    do ctxOriginalRules ctx
                    do v
            r <- pegRulePipeline v pe
            pure r

pegVarStackPipeline :: Pipeline s a ()
pegVarStackPipeline = popUpdateVar >>= \case
    Nothing ->
        pure ()
    Just v -> getAvailableVar v >>= \case
        Just _ ->
            pegVarStackPipeline
        Nothing -> do
            _ <- pegVarPipeline v
            pegVarStackPipeline

pegRulePipeline :: PEG.Var -> PEG.Rule a -> Pipeline s a (LAPEG.Var, SymbolicIntSet.T)
pegRulePipeline v (PEG.Rule alts) = do
    newV <- getNewVar v
    lift do
        modify' \ctx -> ctx
            {
                ctxAvailables = AlignableMap.insert newV
                    do Nothing
                    do ctxAvailables ctx
            }
    newRule <- LARuleBuilder.build do
        forM_ alts \alt -> do
            (newAlt, is) <- lift do pegAltPipeline newV alt
            LARuleBuilder.addAlt is newAlt
    liftBuilder do LAPEGBuilder.addRule newV newRule
    let newAvailable = LAPEG.ruleRange newRule
    lift do
        modify' \ctx -> ctx
            { ctxAvailables = AlignableMap.insert newV
                do Just newAvailable
                do ctxAvailables ctx
            }
    pure (newV, newAvailable)

pegAltPipeline :: LAPEG.Var -> PEG.Alt a -> Pipeline s a (LAPEG.AltNum, SymbolicIntSet.T)
pegAltPipeline ruleV alt = case PEG.altKind alt of
        PEG.AltSeq -> goStraight
        PEG.AltNot -> goNegative
        PEG.AltAnd -> goStraight
    where
        goStraight = case PEG.altUnitSeq alt of
            [] -> do
                n <- newAltNum []
                pure (n, SymbolicIntSet.full)
            u0:us -> do
                (newU0, is) <- goUnit0 u0
                newUs <- forM us \u -> goUnit u
                n <- newAltNum do newU0:newUs
                pure (n, is)

        goNegative = case PEG.altUnitSeq alt of
            [] -> do
                n <- newAltNum [LAPEG.UnitNot]
                pure (n, mempty)
            us@(_:_) -> do
                newUs <- forM us \u -> goUnit u
                n <- newAltNum do LAPEG.UnitNot:newUs
                pure (n, SymbolicIntSet.full)

        newAltNum us = do
            let newAlt = LAPEG.Alt
                    {
                        altVar = ruleV,
                        altUnitSeq = AlignableArray.fromList us,
                        altKind = PEG.altKind alt,
                        altAction = PEG.altAction alt
                    }
            liftBuilder do LAPEGBuilder.addAlt newAlt

        goUnit0 = \case
            PEG.UnitTerminal t ->
                pure (LAPEG.UnitTerminal t, SymbolicIntSet.singleton t)
            PEG.UnitNonTerminal v -> do
                (newV, is) <- pegVarPipeline v
                pure (LAPEG.UnitNonTerminal newV, is)

        goUnit = \case
            PEG.UnitTerminal t ->
                pure do LAPEG.UnitTerminal t
            PEG.UnitNonTerminal v -> do
                pushUpdateVar v
                newV <- getNewVar v
                pure do LAPEG.UnitNonTerminal newV

getNewVar :: PEG.Var -> Pipeline s a LAPEG.Var
getNewVar v = do
    vm0 <- ctxVarMap <$> lift get
    case AlignableMap.lookup v vm0 of
        Just newV ->
            pure newV
        Nothing -> do
            newV <- liftBuilder do LAPEGBuilder.genNewVar
            lift do
                modify' \ctx -> ctx
                    {
                        ctxVarMap = AlignableMap.insert v newV
                            do ctxVarMap ctx
                    }
            pure newV

getAvailableVar :: PEG.Var -> Pipeline s a (Maybe LAPEG.Var)
getAvailableVar v = do
    ctx <- lift get
    case AlignableMap.lookup v do ctxVarMap ctx of
        Nothing ->
            pure Nothing
        Just newV -> case AlignableMap.lookup newV do ctxAvailables ctx of
            Nothing ->
                pure Nothing
            Just Nothing ->
                pure Nothing
            Just (Just _) ->
                pure do Just newV

popUpdateVar :: Pipeline s a (Maybe PEG.Var)
popUpdateVar = do
    ctx <- lift get
    case ctxUpdateVarStack ctx of
        [] ->
            pure Nothing
        v:vs -> do
            lift do put do ctx { ctxUpdateVarStack = vs }
            pure do Just v

pushUpdateVar :: PEG.Var -> Pipeline s a ()
pushUpdateVar v = getAvailableVar v >>= \case
    Just _ ->
        pure ()
    Nothing ->
        lift do
            modify' \ctx -> ctx
                { ctxUpdateVarStack = v:ctxUpdateVarStack ctx }

throwV :: PEG.Var -> Pipeline s a r
throwV v = throwE do AlignableSet.singleton v

liftBuilder :: LAPEGBuilder.T s a Identity r -> Pipeline s a r
liftBuilder builder = do
    ctx <- lift get
    let (x, builderCtx) = runState builder do ctxBuilder ctx
    lift do put do ctx { ctxBuilder = builderCtx }
    pure x
