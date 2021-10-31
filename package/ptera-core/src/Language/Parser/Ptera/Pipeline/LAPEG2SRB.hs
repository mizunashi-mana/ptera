module Language.Parser.Ptera.Pipeline.LAPEG2SRB where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Data.HashMap.Strict                        as HashMap
import qualified Data.List.NonEmpty                         as NonEmpty
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Alignable.Map   as AlignableMap
import qualified Language.Parser.Ptera.Data.Symbolic.IntMap as SymbolicIntMap
import qualified Language.Parser.Ptera.Data.Symbolic.IntSet as SymbolicIntSet
import qualified Language.Parser.Ptera.Machine.LAPEG        as LAPEG
import qualified Language.Parser.Ptera.Machine.SRB          as SRB
import qualified Language.Parser.Ptera.Machine.SRB.Builder  as SRBBuilder

laPeg2Srb :: Enum s => LAPEG.T s a -> SRB.T s a
laPeg2Srb g = runIdentity do
        SRBBuilder.build
            do LAPEG.alts g
            do builder
    where
        builder = do
            initialBuilderCtx <- get
            let initialCtx = Context
                    {
                        ctxBuilder = initialBuilderCtx,
                        ctxInitialVarState = AlignableMap.empty,
                        ctxReduceNotState = AlignableMap.empty,
                        ctxVarMap = AlignableMap.empty,
                        ctxStateMap = HashMap.empty,
                        ctxStateQueue = [],
                        ctxOriginalRules = LAPEG.rules g,
                        ctxAlts = LAPEG.alts g
                    }
            let finalCtx = execState pipeline initialCtx
            put do ctxBuilder finalCtx

        pipeline = do
            forM_
                do EnumMap.assocs do LAPEG.initials g
                do \(s, v) -> laPegInitialPipeline s v
            laPegStateQueuePipeline

type Pipeline s a = State (Context s a)

data Context s a = Context
    {
        ctxBuilder :: SRBBuilder.Context s a,
        ctxInitialVarState :: AlignableMap.T LAPEG.Var SRB.StateNum,
        ctxReduceNotState :: AlignableMap.T LAPEG.AltNum SRB.StateNum,
        ctxVarMap  :: AlignableMap.T LAPEG.Var (SymbolicIntSet.T, SymbolicIntMap.T SRB.StateNum),
        ctxStateMap :: HashMap.HashMap (LAPEG.Position, NonEmpty LAPEG.AltNum) SRB.StateNum,
        ctxStateQueue :: [(SRB.StateNum, LAPEG.Position, NonEmpty LAPEG.AltNum)],
        ctxOriginalRules :: AlignableArray.T LAPEG.Var LAPEG.Rule,
        ctxAlts :: AlignableArray.T LAPEG.AltNum (LAPEG.Alt a)
    }

laPegInitialPipeline :: Enum s => s -> LAPEG.Var -> Pipeline s a ()
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

laPegStateQueuePipeline :: Pipeline s a ()
laPegStateQueuePipeline = do
    ctx <- get
    case ctxStateQueue ctx of
        [] ->
            pure ()
        (sn, p, alts):rest -> do
            put do ctx { ctxStateQueue = rest }
            laPegStatePipeline sn p alts
            laPegStateQueuePipeline

laPegVarPipeline :: LAPEG.Var -> Pipeline s a (SymbolicIntSet.T, SymbolicIntMap.T SRB.StateNum)
laPegVarPipeline v = do
    ctx <- get
    case AlignableMap.lookup v do ctxVarMap ctx of
        Just ss ->
            pure ss
        Nothing -> do
            let r = AlignableArray.forceIndex
                    do ctxOriginalRules ctx
                    do v
            laPegRulePipeline v r

laPegRulePipeline :: LAPEG.Var -> LAPEG.Rule
    -> Pipeline s a (SymbolicIntSet.T, SymbolicIntMap.T SRB.StateNum)
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

laPegEnterStatePipeline :: NonEmpty LAPEG.AltNum
    -> Pipeline s a (SymbolicIntMap.T SRB.StateNum)
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
            mu <- getUnitForAltItem Alignable.initialAlign altn
            s <- case mu of
                Nothing ->
                    pure do SymbolicIntSet.full
                Just (LAPEG.UnitTerminal t) ->
                    pure do SymbolicIntSet.singleton t
                Just (LAPEG.UnitNonTerminal v) -> do
                    (x, _) <- laPegVarPipeline v
                    pure x
                Just LAPEG.UnitNot ->
                    pure do SymbolicIntSet.full
            go1
                do SymbolicIntMap.insertBulk s alts m
                do rest

laPegStatePipeline
    :: SRB.StateNum -> LAPEG.Position -> NonEmpty LAPEG.AltNum
    -> Pipeline s a ()
laPegStatePipeline sn p alts = do
        trans <- laPegTransPipeline p alts
        let st = SRB.MState
                {
                    stateNum = sn,
                    stateTrans = trans,
                    stateAltItems = case alts of
                        alt :| alts' -> toAltItem alt:[toAltItem alt' | alt' <- alts']
                }
        liftBuilder do SRBBuilder.addState st
    where
        toAltItem altn = SRB.AltItem
            {
                altItemAltNum = altn,
                altItemCurPos = p
            }

laPegTransPipeline :: LAPEG.Position -> NonEmpty LAPEG.AltNum
    -> Pipeline s a (SymbolicIntMap.T SRB.Trans)
laPegTransPipeline p0 alts0 = do
        m <- genAltMapForTrans p0 alts0
        let p1 = Alignable.nextAlign p0
        traverse
            do \altItems -> toTrans p1 altItems
            do m
    where
        toTrans p1 altItems = do
            mbackOp <- case altItemsForTransRest altItems of
                [] ->
                    pure Nothing
                ralt:ralts -> do
                    sn <- getStateForAltItems p0 do ralt :| ralts
                    pure do Just do SRB.TransOpPushBackpoint sn
            let withBackOp ops = case mbackOp of
                    Nothing ->
                        ops
                    Just backOp ->
                        backOp:ops
            case altItemsForTransOp altItems of
                AltItemsOpShift -> do
                    let alts = NonEmpty.reverse do altItemsForTransRevAlts altItems
                    sn <- getStateForAltItems p1 alts
                    pure do
                        SRB.TransWithOps
                            do withBackOp [SRB.TransOpShift]
                            do sn
                AltItemsOpEnter v enterSn -> do
                    let alts = NonEmpty.reverse do altItemsForTransRevAlts altItems
                    sn <- getStateForAltItems p1 alts
                    pure do
                        SRB.TransWithOps
                            do withBackOp [SRB.TransOpEnter v do Just sn]
                            do enterSn
                AltItemsOpNot -> do
                    let alts = NonEmpty.reverse do altItemsForTransRevAlts altItems
                    sn <- getStateForAltItems p1 alts
                    notSn <- getStateForReduceNot p0 do NonEmpty.head alts
                    pure do
                        SRB.TransWithOps
                            do withBackOp [SRB.TransOpPushBackpoint notSn]
                            do sn
                AltItemsOpReduce -> do
                    let altn = NonEmpty.last do altItemsForTransRevAlts altItems
                    pure do SRB.TransReduce altn

genAltMapForTrans :: LAPEG.Position -> NonEmpty LAPEG.AltNum
    -> Pipeline s a (SymbolicIntMap.T AltItemsForTrans)
genAltMapForTrans p alts0 = go SymbolicIntMap.empty alts0 where
    go m0 (alt :| rest) = getUnitForAltItem p alt >>= \case
        Nothing -> do
            let m1 = SymbolicIntMap.alterBulk
                    do \case
                        e@(Just altItems) | hasRest altItems ->
                            e
                        Just altItems -> Just do
                            altItems
                                {
                                    altItemsForTransRest = alt:rest
                                }
                        Nothing -> Just do
                            AltMapForTrans
                                { altItemsForTransOp = AltItemsOpReduce
                                , altItemsForTransRevAlts = pure alt
                                , altItemsForTransRest = []
                                }
                    do SymbolicIntSet.full
                    do m0
            pure m1
        Just (LAPEG.UnitTerminal t) -> do
            let m1 = SymbolicIntMap.alter
                    do \case
                        e@(Just altItems) | hasRest altItems ->
                            e
                        Just altItems -> case altItemsForTransOp altItems of
                            AltItemsOpShift -> Just do
                                altItems
                                    {
                                        altItemsForTransRevAlts = NonEmpty.cons alt
                                            do altItemsForTransRevAlts altItems
                                    }
                            _ -> Just do
                                altItems
                                    {
                                        altItemsForTransRest = alt:rest
                                    }
                        Nothing -> Just do
                            AltMapForTrans
                                { altItemsForTransOp = AltItemsOpShift
                                , altItemsForTransRevAlts = pure alt
                                , altItemsForTransRest = []
                                }
                    do t
                    do m0
            pure m1
        Just (LAPEG.UnitNonTerminal v) -> do
            (_, vm) <- laPegVarPipeline v
            let m1 = SymbolicIntMap.merge
                    do \altItems sn -> case altItemsForTransOp altItems of
                        _ | hasRest altItems ->
                            Just altItems
                        AltItemsOpEnter v' sn' | v == v' && sn == sn' -> Just do
                            altItems
                                {
                                    altItemsForTransRevAlts = NonEmpty.cons alt
                                        do altItemsForTransRevAlts altItems
                                }
                        _ -> Just do
                            altItems
                                {
                                    altItemsForTransRest = alt:rest
                                }
                    do \altItems -> Just altItems
                    do \sn -> Just do
                        AltMapForTrans
                            { altItemsForTransOp = AltItemsOpEnter v sn
                            , altItemsForTransRevAlts = pure alt
                            , altItemsForTransRest = []
                            }
                    do m0
                    do vm
            pure m1
        Just LAPEG.UnitNot -> do
            let m1 = SymbolicIntMap.alterBulk
                    do \case
                        e@(Just altItems) | hasRest altItems ->
                            e
                        Just altItems -> Just do
                            altItems
                                {
                                    altItemsForTransRest = alt:rest
                                }
                        Nothing -> Just do
                            AltMapForTrans
                                { altItemsForTransOp = AltItemsOpNot
                                , altItemsForTransRevAlts = pure alt
                                , altItemsForTransRest = rest
                                }
                    do SymbolicIntSet.full
                    do m0
            pure m1

    hasRest altItems = not do null do altItemsForTransRest altItems

data AltItemsForTrans = AltMapForTrans
    {
        altItemsForTransOp      :: AltItemsOpForTrans,
        altItemsForTransRevAlts :: NonEmpty LAPEG.AltNum,
        altItemsForTransRest    :: [LAPEG.AltNum]
    }

data AltItemsOpForTrans
    = AltItemsOpShift
    | AltItemsOpEnter LAPEG.Var SRB.StateNum
    | AltItemsOpNot
    | AltItemsOpReduce
    deriving (Eq, Show)

getStateForAltItems :: LAPEG.Position -> NonEmpty LAPEG.AltNum -> Pipeline s a SRB.StateNum
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

getStateForReduceNot :: LAPEG.Position -> LAPEG.AltNum -> Pipeline s a SRB.StateNum
getStateForReduceNot p alt = do
    ctx <- get
    case AlignableMap.lookup alt do ctxReduceNotState ctx of
        Just s ->
            pure s
        Nothing -> do
            s <- liftBuilder SRBBuilder.genNewStateNum
            put do
                ctx
                    {
                        ctxReduceNotState = AlignableMap.insert alt s
                            do ctxReduceNotState ctx
                    }
            let altItem = SRB.AltItem
                    {
                        altItemAltNum = alt,
                        altItemCurPos = p
                    }
            let st = SRB.MState
                    { stateNum = s
                    , stateTrans = SymbolicIntMap.insertBulk
                        do SymbolicIntSet.full
                        do SRB.TransReduceNot alt
                        do SymbolicIntMap.empty
                    , stateAltItems = [altItem]
                    }
            liftBuilder do SRBBuilder.addState st
            pure s

getUnitForAltItem :: LAPEG.Position -> LAPEG.AltNum -> Pipeline s a (Maybe LAPEG.Unit)
getUnitForAltItem p altn = do
    alt <- getAlt altn
    let us = LAPEG.altUnitSeq alt
    pure do AlignableArray.index us p

getAlt :: LAPEG.AltNum -> Pipeline s a (LAPEG.Alt a)
getAlt altn = do
    ctx <- get
    let alts = ctxAlts ctx
    pure do AlignableArray.forceIndex alts altn

liftBuilder :: SRBBuilder.T s a Identity r -> Pipeline s a r
liftBuilder builder = do
    ctx <- get
    let (x, builderCtx) = runIdentity
            do runStateT builder do ctxBuilder ctx
    put do ctx { ctxBuilder = builderCtx }
    pure x