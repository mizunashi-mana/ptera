module Language.Parser.Ptera.Pipeline.PEG2SRB where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Data.HashMap.Strict                        as HashMap
import qualified Data.List.NonEmpty                         as NonEmpty
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Alignable.Map   as AlignableMap
import qualified Language.Parser.Ptera.Data.Symbolic.IntMap as SymbolicIntMap
import qualified Language.Parser.Ptera.Data.Symbolic.IntSet as SymbolicIntSet
import qualified Language.Parser.Ptera.Machine.PEG          as PEG
import qualified Language.Parser.Ptera.Machine.SRB          as SRB
import qualified Language.Parser.Ptera.Machine.SRB.Builder  as SRBBuilder

laPeg2Srb :: Enum start
    => PEG.T start varDoc altDoc a -> SRB.T start varDoc altDoc a
laPeg2Srb g = runIdentity do
        SRBBuilder.build
            do PEG.vars g
            do PEG.alts g
            do builder
    where
        builder = do
            initialBuilderCtx <- get
            let initialCtx = Context
                    { ctxBuilder = initialBuilderCtx
                    , ctxInitialVarState = AlignableMap.empty
                    , ctxReduceNotState = AlignableMap.empty
                    , ctxVarMap = AlignableMap.empty
                    , ctxStateMap = HashMap.empty
                    , ctxStateQueue = []
                    , ctxOriginalRules = PEG.rules g
                    , ctxOriginalAlts = PEG.alts g
                    }
            let finalCtx = execState pipeline initialCtx
            put do ctxBuilder finalCtx

        pipeline = do
            forM_
                do EnumMap.assocs do PEG.initials g
                do \(s, v) -> laPegInitialPipeline s v
            laPegStateQueuePipeline

type Pipeline start altDoc a = State (Context start altDoc a)

data Context start altDoc a = Context
    { ctxBuilder :: SRBBuilder.Context start a
    , ctxInitialVarState :: AlignableMap.T PEG.VarNum SRB.StateNum
    , ctxReduceNotState :: AlignableMap.T PEG.AltNum SRB.StateNum
    , ctxVarMap  :: AlignableMap.T PEG.VarNum (SymbolicIntSet.T, SymbolicIntMap.T (Bool, SRB.StateNum))
    , ctxStateMap :: HashMap.HashMap (PEG.Position, NonEmpty PEG.AltNum) SRB.StateNum
    , ctxStateQueue :: [(SRB.StateNum, PEG.Position, NonEmpty PEG.AltNum)]
    , ctxOriginalRules :: AlignableArray.T PEG.VarNum PEG.Rule
    , ctxOriginalAlts :: AlignableArray.T PEG.AltNum (PEG.Alt altDoc a)
    }

laPegInitialPipeline :: Enum start
    => start -> PEG.VarNum -> Pipeline start altDoc a ()
laPegInitialPipeline s v = do
    m0 <- ctxInitialVarState <$> get
    newSn <- case AlignableMap.lookup v m0 of
        Just sn ->
            pure sn
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
                        do \(needBack, to) -> SRB.TransWithOps [SRB.TransOpEnter v needBack Nothing] to
                        do m
                    , stateAltItems = []
                    }
            liftBuilder do SRBBuilder.addState st
            pure sn
    liftBuilder do SRBBuilder.registerInitial s newSn

laPegStateQueuePipeline :: Pipeline start altDoc a ()
laPegStateQueuePipeline = do
    ctx <- get
    case ctxStateQueue ctx of
        [] ->
            pure ()
        (sn, p, alts):rest -> do
            put do ctx { ctxStateQueue = rest }
            laPegStatePipeline sn p alts
            laPegStateQueuePipeline

laPegVarPipeline :: PEG.VarNum
    -> Pipeline start altDoc a (SymbolicIntSet.T, SymbolicIntMap.T (Bool, SRB.StateNum))
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

laPegRulePipeline
    :: PEG.VarNum -> PEG.Rule
    -> Pipeline start altDoc a (SymbolicIntSet.T, SymbolicIntMap.T (Bool, SRB.StateNum))
laPegRulePipeline v r = do
    sm <- case PEG.ruleAlts r of
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

laPegEnterStatePipeline
    :: NonEmpty PEG.AltNum
    -> Pipeline start altDoc a (SymbolicIntMap.T (Bool, SRB.StateNum))
laPegEnterStatePipeline = \alts -> go do revTails [] alts where
    revTails accs = \case
        alts@(_:|[]) -> alts:accs
        alts@(_:|alt1:rest) -> revTails
            do alts:accs
            do alt1 :| rest

    go altss = do
        m <- go1 SymbolicIntMap.empty altss
        traverse
            do \alts -> do
                needBack <- isNeedBackAlts alts
                sn <- getStateForAltItems Alignable.initialAlign alts
                pure (needBack, sn)
            do m

    go1 m = \case
        [] ->
            pure m
        alts@(altn :| _):rest -> do
            mu <- getUnitForAltItem Alignable.initialAlign altn
            s <- case mu of
                Nothing ->
                    pure do SymbolicIntSet.full
                Just (PEG.UnitTerminal t) ->
                    pure do SymbolicIntSet.singleton t
                Just (PEG.UnitNonTerminal v) -> do
                    (x, _) <- laPegVarPipeline v
                    pure x
                Just PEG.UnitNot ->
                    pure do SymbolicIntSet.full
            go1
                do SymbolicIntMap.insertBulk s alts m
                do rest

laPegStatePipeline
    :: SRB.StateNum -> PEG.Position -> NonEmpty PEG.AltNum
    -> Pipeline start altDoc a ()
laPegStatePipeline sn p alts = do
        trans <- laPegTransPipeline p alts
        let st = SRB.MState
                { stateNum = sn
                , stateTrans = trans
                , stateAltItems = case alts of
                    alt :| alts' -> toAltItem alt:[toAltItem alt' | alt' <- alts']
                }
        liftBuilder do SRBBuilder.addState st
    where
        toAltItem altn = SRB.AltItem
            {
                altItemAltNum = altn,
                altItemCurPos = p
            }

laPegTransPipeline
    :: PEG.Position -> NonEmpty PEG.AltNum
    -> Pipeline start altDoc a (SymbolicIntMap.T SRB.Trans)
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
                AltItemsOpEnter v needBack enterSn -> do
                    let alts = NonEmpty.reverse do altItemsForTransRevAlts altItems
                    sn <- getStateForAltItems p1 alts
                    pure do
                        SRB.TransWithOps
                            do withBackOp [SRB.TransOpEnter v needBack do Just sn]
                            do enterSn
                AltItemsOpNot -> do
                    let alts = NonEmpty.reverse do altItemsForTransRevAlts altItems
                    sn <- getStateForAltItems p1 alts
                    let notAlt = NonEmpty.head alts
                    pure do
                        SRB.TransWithOps
                            do withBackOp [SRB.TransOpHandleNot notAlt]
                            do sn
                AltItemsOpReduce -> do
                    let altn = NonEmpty.last do altItemsForTransRevAlts altItems
                    pure do SRB.TransReduce altn

genAltMapForTrans
    :: PEG.Position -> NonEmpty PEG.AltNum
    -> Pipeline start altDoc a (SymbolicIntMap.T AltItemsForTrans)
genAltMapForTrans p (alt0 :| alts0) = go SymbolicIntMap.empty do alt0:alts0 where
    go m0 = \case
        [] ->
            pure m0
        alt:rest -> do
            m1 <- goAlt m0 alt rest
            go m1 rest

    goAlt m0 alt rest = getUnitForAltItem p alt >>= \case
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
        Just (PEG.UnitTerminal t) -> do
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
        Just (PEG.UnitNonTerminal v) -> do
            (_, vm) <- laPegVarPipeline v
            let m1 = SymbolicIntMap.merge
                    do \altItems (needBack, sn) -> case altItemsForTransOp altItems of
                        _ | hasRest altItems ->
                            Just altItems
                        transOp@AltItemsOpEnter{} | transOp == AltItemsOpEnter v needBack sn ->
                            Just do
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
                    do \(needBack, sn) -> Just do
                        AltMapForTrans
                            { altItemsForTransOp = AltItemsOpEnter v needBack sn
                            , altItemsForTransRevAlts = pure alt
                            , altItemsForTransRest = []
                            }
                    do m0
                    do vm
            pure m1
        Just PEG.UnitNot -> do
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
        altItemsForTransRevAlts :: NonEmpty PEG.AltNum,
        altItemsForTransRest    :: [PEG.AltNum]
    }
    deriving (Eq, Show)

data AltItemsOpForTrans
    = AltItemsOpShift
    | AltItemsOpEnter PEG.VarNum Bool SRB.StateNum
    | AltItemsOpNot
    | AltItemsOpReduce
    deriving (Eq, Show)

getStateForAltItems
    :: PEG.Position -> NonEmpty PEG.AltNum
    -> Pipeline start altDoc a SRB.StateNum
getStateForAltItems p alts = do
    m <- ctxStateMap <$> get
    case HashMap.lookup (p, alts) m of
        Just sn ->
            pure sn
        Nothing -> do
            sn <- liftBuilder SRBBuilder.genNewStateNum
            modify' \ctx -> ctx
                { ctxStateMap = HashMap.insert (p, alts) sn
                    do ctxStateMap ctx
                , ctxStateQueue = (sn, p, alts):ctxStateQueue ctx
                }
            pure sn

isNeedBackAlts :: NonEmpty PEG.AltNum -> Pipeline start altDoc a Bool
isNeedBackAlts = \(altn :| rest) -> go altn rest where
    go altn0 rest = do
        alt0 <- getAlt altn0
        case PEG.altKind alt0 of
            PEG.AltNot ->
                pure True
            PEG.AltAnd ->
                pure True
            PEG.AltSeq -> case rest of
                [] ->
                    pure False
                altn1:alts ->
                    go altn1 alts

getUnitForAltItem
    :: PEG.Position -> PEG.AltNum
    -> Pipeline start altDoc a (Maybe PEG.Unit)
getUnitForAltItem p altn = do
    alt <- getAlt altn
    let us = PEG.altUnitSeq alt
    pure do AlignableArray.index us p

getAlt :: PEG.AltNum -> Pipeline start altDoc a (PEG.Alt altDoc a)
getAlt altn = do
    ctx <- get
    let alts = ctxOriginalAlts ctx
    pure do AlignableArray.forceIndex alts altn

liftBuilder :: SRBBuilder.T start a Identity r -> Pipeline start altDoc a r
liftBuilder builder = do
    ctx <- get
    let (x, builderCtx) = runIdentity
            do runStateT builder do ctxBuilder ctx
    put do ctx { ctxBuilder = builderCtx }
    pure x
