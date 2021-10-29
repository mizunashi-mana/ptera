module Language.Parser.Ptera.Pipeline.LAPEG2SRB where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Data.Alignable as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Alignable.Map as AlignableMap
import qualified Language.Parser.Ptera.Data.Symbolic.IntMap as SymbolicIntMap
import qualified Language.Parser.Ptera.Data.Symbolic.IntSet as SymbolicIntSet
import qualified Language.Parser.Ptera.Machine.PEG          as PEG
import qualified Language.Parser.Ptera.Machine.LAPEG          as LAPEG
import qualified Language.Parser.Ptera.Machine.SRB         as SRB
import qualified Language.Parser.Ptera.Machine.SRB.Builder as SRBBuilder
import qualified Data.HashMap.Strict as HashMap

laPeg2Srb :: LAPEG.T a -> SRB.T a
laPeg2Srb g = undefined g


type Pipeline a = State (Context a)

data Context a = Context
    {
        ctxBuilder :: SRBBuilder.Context a,
        ctxInitialVarState :: AlignableMap.T LAPEG.Var SRB.StateNum,
        ctxVarMap  :: AlignableMap.T LAPEG.Var (SymbolicIntSet.T, SymbolicIntMap.T SRB.StateNum),
        ctxStateMap :: HashMap.HashMap (LAPEG.Position, NonEmpty LAPEG.AltNum) SRB.StateNum,
        ctxStateQueue :: [(SRB.StateNum, LAPEG.Position, NonEmpty LAPEG.AltNum)],
        ctxOriginalRules :: AlignableArray.T LAPEG.Var LAPEG.Rule,
        ctxAlts :: AlignableArray.T LAPEG.AltNum (LAPEG.Alt a)
    }

laPegInitialPipeline :: PEG.StartPoint -> LAPEG.Var -> Pipeline a ()
laPegInitialPipeline s v = do
    m0 <- ctxInitialVarState <$> get
    case AlignableMap.lookup v m0 of
        Just sn ->
            liftBuilder do SRBBuilder.registerInitial s sn
        Nothing -> do
            sn <- liftBuilder do SRBBuilder.genNewStateNum
            modify' \ctx -> ctx
                {
                    ctxInitialVarState = AlignableMap.insert v sn
                        do ctxInitialVarState ctx
                }
            (_, m) <- laPegVarPipeline v
            let st = SRB.MState
                    { stateNum = sn
                    , stateTrans = fmap
                        do \to -> SRB.TransWithOps [SRB.TransOpEnter v Nothing] to
                        do m
                    , stateAltItems = []
                    }
            liftBuilder do SRBBuilder.addState st

laPegStateQueuePipeline :: Pipeline a ()
laPegStateQueuePipeline = do
    ctx <- get
    case ctxStateQueue ctx of
        [] ->
            pure ()
        (sn, p, alts):rest -> do
            put do ctx { ctxStateQueue = rest }
            laPegStatePipeline sn p alts
            laPegStateQueuePipeline

laPegVarPipeline :: LAPEG.Var -> Pipeline a (SymbolicIntSet.T, SymbolicIntMap.T SRB.StateNum)
laPegVarPipeline v = do
    ctx <- get
    case AlignableMap.lookup v do ctxVarMap ctx of
        Just ss ->
            pure ss
        Nothing -> do
            let r = AlignableArray.index
                    do ctxOriginalRules ctx
                    do v
            laPegRulePipeline v r

laPegRulePipeline :: LAPEG.Var -> LAPEG.Rule -> Pipeline a (SymbolicIntSet.T, SymbolicIntMap.T SRB.StateNum)
laPegRulePipeline v r = do
    sm <- case LAPEG.ruleAlts r of
        [] ->
            pure SymbolicIntMap.empty
        alt:alts ->
            laPegEnterStatePipeline do alt :| alts
    let ss = (SymbolicIntMap.keys sm, sm)
    modify' \ctx -> ctx
        {
            ctxVarMap = AlignableMap.insert v ss
                do ctxVarMap ctx
        }
    pure ss

laPegEnterStatePipeline :: NonEmpty LAPEG.AltNum -> Pipeline a (SymbolicIntMap.T SRB.StateNum)
laPegEnterStatePipeline = \alts -> go do revTails [] alts where
    revTails accs = \case
        alts@(_:|[]) -> alts:accs
        alts@(_:|alt1:rest) -> revTails
            do alts:accs
            do alt1 :| rest

    go altss = do
        m <- go1 SymbolicIntMap.empty altss
        traverse
            do \alts -> getStateForAltItems Alignable.initialAlign alts
            do m

    go1 m = \case
        [] ->
            pure m
        alts@(altn :| _):rest -> do
            unit <- getUnitForAltItem Alignable.initialAlign altn
            s <- case unit of
                LAPEG.UnitTerminal t ->
                    pure do SymbolicIntSet.singleton t
                LAPEG.UnitNonTerminal v -> do
                    (x, _) <- laPegVarPipeline v
                    pure x
            go1
                do SymbolicIntMap.insertBulk s alts m
                do rest

laPegStatePipeline
    :: SRB.StateNum -> LAPEG.Position -> NonEmpty LAPEG.AltNum
    -> Pipeline a ()
laPegStatePipeline s p alts = undefined s p alts

getStateForAltItems :: LAPEG.Position -> NonEmpty LAPEG.AltNum -> Pipeline a SRB.StateNum
getStateForAltItems p alts = do
    ctx <- get
    case HashMap.lookup (p, alts) do ctxStateMap ctx of
        Just s ->
            pure s
        Nothing -> do
            s <- liftBuilder SRBBuilder.genNewStateNum
            put do
                ctx
                    {
                        ctxStateMap = HashMap.insert (p, alts) s
                            do ctxStateMap ctx
                    }
            pure s

getUnitForAltItem :: LAPEG.Position -> LAPEG.AltNum -> Pipeline a LAPEG.Unit
getUnitForAltItem p altn = do
    ctx <- get
    let alt = AlignableArray.index
            do ctxAlts ctx
            do altn
    let unit = AlignableArray.index
            do LAPEG.altUnitSeq alt
            do p
    pure unit

liftBuilder :: SRBBuilder.T a Identity r -> Pipeline a r
liftBuilder builder = do
    ctx <- get
    let (x, builderCtx) = runIdentity
            do runStateT builder do ctxBuilder ctx
    put do ctx { ctxBuilder = builderCtx }
    pure x
