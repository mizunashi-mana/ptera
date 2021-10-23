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
                ctxOriginalRules = PEG.rules g
            }
    (_, finalCtx) <- lift do
        runStateT
            do pipeline do PEG.initials g
            do initialCtx
    put do ctxBuilder finalCtx
    where
        pipeline inits = runPipelinesWithFallbacks
            do EnumMap.assocs inits
            \(s, v) -> pegInitialPipeline s v


type Pipeline a = StateT (Context a) (Except (AlignableSet.T PEG.Var))

data Context a = Context
    {
        ctxBuilder :: LAPEGBuilder.Context a,
        ctxVarMap :: AlignableMap.T PEG.Var (Maybe (LAPEG.Var, SymbolicIntSet.T)),
        ctxOriginalRules :: AlignableArray.T PEG.Var (PEG.PE a)
    }

pegInitialPipeline :: PEG.StartPoint -> PEG.Var -> Pipeline a ()
pegInitialPipeline s v = do
    ctx <- get
    newV <- case AlignableMap.lookup v do ctxVarMap ctx of
        Just (Just (newV, _)) ->
            pure newV
        Just Nothing ->
            goRule ctx
        Nothing ->
            goRule ctx
    liftBuilder do LAPEGBuilder.registerInitial s newV
    where
        goRule ctx = do
            let pe = AlignableArray.index
                    do ctxOriginalRules ctx
                    do v
            (newV, _) <- pegRulePipeline v pe
            pure newV

pegRulePipeline :: PEG.Var -> PEG.PE a -> Pipeline a (LAPEG.Var, SymbolicIntSet.T)
pegRulePipeline v (PEG.PE alts) = do
    newV <- getNewVar
    modify' \ctx -> ctx
        {
            ctxVarMap = AlignableMap.insert v Nothing do ctxVarMap ctx
        }
    newPe <- LAPEBuilder.build do
        forM_ alts \alt -> do
            (newAlt, is) <- lift do pegAltPipeline alt
            LAPEBuilder.addAlt is newAlt
    liftBuilder do LAPEGBuilder.addRule newV newPe
    let r = (newV, LAPEG.available newPe)
    modify' \ctx -> ctx
        {
            ctxVarMap = AlignableMap.insert v
                do Just r
                do ctxVarMap ctx
        }
    pure r

pegAltPipeline :: PEG.Alt a -> Pipeline a (LAPEG.Alt a, SymbolicIntSet.T)
pegAltPipeline alt = case PEG.altUnitSeq alt of
    [] ->
        pure (newAlt [], SymbolicIntSet.full)
    u0:us -> do
        (newU0, is) <- goUnit u0
        newUs <- forM us \u -> do
            (newU, _) <- goUnit u
            pure newU
        pure (newAlt do newU0:newUs, is)
    where
        newAlt us = LAPEG.Alt
            {
                altUnitSeq = us,
                altKind = PEG.altKind alt,
                altAction = PEG.altAction alt
            }

        goUnit = \case
            PEG.UnitTerminal t ->
                pure (LAPEG.UnitTerminal t, SymbolicIntSet.singleton t)
            PEG.UnitNonTerminal v -> do
                ctx <- get
                case AlignableMap.lookup v do ctxVarMap ctx of
                    Just Nothing ->
                        throwV v
                    Just (Just (newV, is)) ->
                        pure (LAPEG.UnitNonTerminal newV, is)
                    Nothing -> do
                        let origRules = ctxOriginalRules ctx
                        let pe = AlignableArray.index origRules v
                        (newV, is) <- pegRulePipeline v pe
                        pure (LAPEG.UnitNonTerminal newV, is)

getNewVar :: Pipeline a LAPEG.Var
getNewVar = liftBuilder do LAPEGBuilder.getNewVar

throwV :: PEG.Var -> Pipeline a r
throwV v = lift do throwE do AlignableSet.singleton v

runPipelinesWithFallbacks :: [i] -> (i -> Pipeline a ()) -> Pipeline a ()
runPipelinesWithFallbacks is f = mapStateT
    do \(Identity (mvs, s)) -> case fold mvs of
        Nothing -> pure ((), s)
        Just vs -> throwE vs
    do forM is \i -> do
        ctx0 <- get
        let mctx1 = execStateT
                do f i
                do ctx0
        case runExcept mctx1 of
            Left vs ->
                pure do Just vs
            Right ctx1 -> do
                put ctx1
                pure Nothing

liftBuilder :: LAPEGBuilder.T a Identity r -> Pipeline a r
liftBuilder builder = do
    ctx <- get
    let (x, builderCtx) = runState builder do ctxBuilder ctx
    put do ctx { ctxBuilder = builderCtx }
    pure x
