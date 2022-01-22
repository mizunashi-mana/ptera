module Language.Parser.Ptera.Pipeline.PEG2LAPEG where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                         as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable.Array  as AlignableArray
import qualified Language.Parser.Ptera.Data.Alignable.Map    as AlignableMap
import qualified Language.Parser.Ptera.Data.Alignable.Set    as AlignableSet
import qualified Language.Parser.Ptera.Data.Symbolic.IntSet  as SymbolicIntSet
import qualified Language.Parser.Ptera.Machine.LAPEG         as LAPEG
import qualified Language.Parser.Ptera.Machine.LAPEG.Builder as LAPEGBuilder
import qualified Language.Parser.Ptera.Machine.PEG           as PEG


peg2LaPeg :: Enum start
    => PEG.T start varDoc altDoc a
    -> Except (AlignableSet.T PEG.VarNum) (LAPEG.T start varDoc altDoc a)
peg2LaPeg g = LAPEGBuilder.build builder where
    builder = do
        initialCtxBuilder <- get
        let initialCtx = Context
                { ctxBuilder = initialCtxBuilder
                , ctxVarMap = AlignableMap.empty
                , ctxAvailableRuleRanges = AlignableMap.empty
                , ctxUpdateRuleStack = []
                , ctxOriginalVars = PEG.vars g
                , ctxOriginalRules = PEG.rules g
                , ctxOriginalAlts = PEG.alts g
                }
        let (mx, finalCtx) = runState
                do runExceptT do pipeline do PEG.initials g
                do initialCtx
        case mx of
            Left vs -> lift do throwE vs
            Right{} -> put do ctxBuilder finalCtx

    pipeline inits = do
        rvs <- foldlM
            do \vs1 (s, v) -> catchE
                do
                    pegInitialPipeline s v
                    pure vs1
                \vs2 -> do
                    lift do
                        modify' \ctx -> ctx
                            { ctxAvailableRuleRanges = AlignableMap.empty
                            , ctxUpdateRuleStack = []
                            }
                    pure do AlignableSet.union vs1 vs2
            do AlignableSet.empty
            do EnumMap.assocs inits
        if AlignableSet.null rvs
            then pure ()
            else throwE rvs

type Pipeline start varDoc altDoc a =
    ExceptT (AlignableSet.T PEG.VarNum) (State (Context start varDoc altDoc a))

data Context start varDoc altDoc a = Context
    { ctxBuilder        :: LAPEGBuilder.Context start varDoc altDoc a
    , ctxVarMap         :: AlignableMap.T PEG.VarNum LAPEG.VarNum
    , ctxAvailableRuleRanges     :: AlignableMap.T LAPEG.VarNum (Maybe LAPEG.HeadRange)
    , ctxUpdateRuleStack :: [(LAPEG.VarNum, LAPEG.HeadRange, [PEG.Alt altDoc a])]
    , ctxOriginalVars :: AlignableArray.T PEG.VarNum (PEG.Var varDoc)
    , ctxOriginalRules  :: AlignableArray.T PEG.VarNum PEG.Rule
    , ctxOriginalAlts  :: AlignableArray.T PEG.AltNum (PEG.Alt altDoc a)
    }

pegInitialPipeline :: Enum start
    => start -> PEG.VarNum -> Pipeline start varDoc altDoc a ()
pegInitialPipeline s v = do
    newV <- getAvailableVar v >>= \case
        Just x ->
            pure x
        Nothing -> do
            (x, _) <- pegVarPipeline v
            pure x
    pegRuleStackPipeline
    liftBuilder do LAPEGBuilder.addInitial s newV

pegRuleStackPipeline :: Pipeline start varDoc altDoc a ()
pegRuleStackPipeline = popUpdateRuleItem >>= \case
    Nothing ->
        pure ()
    Just (newV, newRange, rule) -> do
        pegRulePipeline newV newRange rule
        pegRuleStackPipeline

pegVarPipeline
    :: PEG.VarNum -> Pipeline start varDoc altDoc a (LAPEG.VarNum, LAPEG.HeadRange)
pegVarPipeline v = do
    newV <- getNewVar v
    availableRuleRanges <- getCtx ctxAvailableRuleRanges
    case AlignableMap.lookup newV availableRuleRanges of
        Nothing ->
            goVarUpdate newV
        Just Nothing ->
            throwV v
        Just (Just hr) ->
            pure (newV, hr)
    where
        goVarUpdate newV = do
            pegRules <- getCtx ctxOriginalRules
            let rule = AlignableArray.forceIndex pegRules v
            hr <- pegRuleHeadRangePipeline newV rule
            pure (newV, hr)

pegRuleHeadRangePipeline
    :: LAPEG.VarNum -> PEG.Rule
    -> Pipeline start varDoc altDoc a LAPEG.HeadRange
pegRuleHeadRangePipeline newV (PEG.Rule altns) = do
    originalAlts <- getCtx ctxOriginalAlts
    let alts = [ AlignableArray.forceIndex originalAlts altn | altn <- altns ]
    startUpdateAvailableRuleRange newV
    newRange <- foldM
        do \hr0 alt -> do
            hr <- pegAltHeadRangePipeline alt
            pure do hr0 <> hr
        do mempty
        do alts
    saveNewRuleRange newV newRange
    pushUpdateRuleItem newV newRange alts
    pure newRange

pegAltHeadRangePipeline
    :: PEG.Alt altDoc a -> Pipeline start varDoc altDoc a LAPEG.HeadRange
pegAltHeadRangePipeline alt =
    case PEG.altKind alt of
        PEG.AltSeq -> goStraight
        PEG.AltNot -> goNegative
        PEG.AltAnd -> goStraight
    where
        goStraight = goUnits0 do PEG.altUnitSeq alt

        goNegative = do
            hr <- goUnits0 do PEG.altUnitSeq alt
            let notHr = if
                    | LAPEG.headRangeEpsilon hr ->
                        mempty
                    | otherwise ->
                        LAPEG.HeadRange
                            { headRangeEpsilon = True
                            , headRangeConsume = SymbolicIntSet.full
                            }
            pure notHr

        goUnits0 us = goUnits mempty us

        goUnits consumeRange0 = \case
            [] -> do
                let hr = LAPEG.HeadRange
                        { headRangeEpsilon = True
                        , headRangeConsume = consumeRange0
                        }
                pure hr
            u:us -> do
                (_, hr) <- pegUnitPipeline u
                let consumeRange1 = consumeRange0 <> LAPEG.headRangeConsume hr
                if LAPEG.headRangeEpsilon hr
                    then
                        goUnits consumeRange1 us
                    else do
                        let hr1 = LAPEG.HeadRange
                                { headRangeEpsilon = False
                                , headRangeConsume = consumeRange1
                                }
                        pure hr1

pegRulePipeline
    :: LAPEG.VarNum -> LAPEG.HeadRange -> [PEG.Alt altDoc a]
    -> Pipeline start varDoc altDoc a ()
pegRulePipeline newV newRange alts = do
    newAlts <- forM alts \alt -> pegAltPipeline newV alt
    let newRule = LAPEG.Rule
            { ruleRange = newRange
            , ruleAlts = newAlts
            }
    liftBuilder do LAPEGBuilder.addRule newV newRule

pegAltPipeline
    :: LAPEG.VarNum -> PEG.Alt altDoc a
    -> Pipeline start varDoc altDoc a LAPEG.AltNum
pegAltPipeline newV alt =
    case PEG.altKind alt of
        PEG.AltSeq -> goStraight
        PEG.AltNot -> goNegative
        PEG.AltAnd -> goStraight
    where
        goStraight = do
            (_, newUs) <- goUnits do PEG.altUnitSeq alt
            newAlt <- genNewAltNum newUs
            pure newAlt

        goNegative = do
            (hr, newUs) <- goUnits do PEG.altUnitSeq alt
            let notHr = if
                    | LAPEG.headRangeEpsilon hr ->
                        mempty
                    | otherwise ->
                        LAPEG.HeadRange
                            { headRangeEpsilon = True
                            , headRangeConsume = SymbolicIntSet.full
                            }
            newAlt <- genNewAltNum do (notHr, LAPEG.UnitNot):newUs
            pure newAlt

        genNewAltNum newUs = do
            let newAlt = LAPEG.Alt
                    { altVar = newV
                    , altUnitSeqWithLookAHead = AlignableArray.fromList newUs
                    , altKind = PEG.altKind alt
                    , altAction = PEG.altAction alt
                    , altHelp = PEG.altHelp alt
                    }
            liftBuilder do LAPEGBuilder.genNewAlt newAlt

        goUnits us = do
            let hr0 = LAPEG.HeadRange
                    { headRangeEpsilon = True
                    , headRangeConsume = mempty
                    }
            goRevUnits hr0 [] do reverse us

        goRevUnits postRange newUs = \case
            [] ->
                pure (postRange, newUs)
            u:revUs -> do
                (newU, hrU) <- pegUnitPipeline u
                let hrUWithPost = if LAPEG.headRangeEpsilon hrU
                        then LAPEG.HeadRange
                            { headRangeEpsilon =
                                LAPEG.headRangeEpsilon postRange
                            , headRangeConsume =
                                LAPEG.headRangeConsume hrU <> LAPEG.headRangeConsume postRange
                            }
                        else hrU
                goRevUnits hrUWithPost ((hrUWithPost, newU):newUs) revUs

pegUnitPipeline
    :: PEG.Unit -> Pipeline start varDoc altDoc a (LAPEG.Unit, LAPEG.HeadRange)
pegUnitPipeline = \case
    PEG.UnitTerminal t -> do
        let hr = LAPEG.HeadRange
                { headRangeEpsilon = False
                , headRangeConsume = SymbolicIntSet.singleton t
                }
        pure (LAPEG.UnitTerminal t, hr)
    PEG.UnitNonTerminal v -> do
        (newV, hr) <- pegVarPipeline v
        pure (LAPEG.UnitNonTerminal newV, hr)

getNewVar :: PEG.VarNum -> Pipeline start varDoc altDoc a LAPEG.VarNum
getNewVar vn = do
    vm0 <- getCtx ctxVarMap
    case AlignableMap.lookup vn vm0 of
        Just newV ->
            pure newV
        Nothing -> do
            originalVars <- getCtx ctxOriginalVars
            let v = AlignableArray.forceIndex originalVars vn
            newV <- liftBuilder do LAPEGBuilder.genNewVar v
            lift do
                modify' \ctx -> ctx
                    {
                        ctxVarMap = AlignableMap.insert vn newV
                            do ctxVarMap ctx
                    }
            pure newV

startUpdateAvailableRuleRange :: LAPEG.VarNum -> Pipeline start varDoc altDoc a ()
startUpdateAvailableRuleRange newV = lift do
    modify' \ctx -> ctx
        { ctxAvailableRuleRanges = AlignableMap.insert newV
            do Nothing
            do ctxAvailableRuleRanges ctx
        }

saveNewRuleRange
    :: LAPEG.VarNum -> LAPEG.HeadRange
    -> Pipeline start varDoc altDoc a ()
saveNewRuleRange newV hr = lift do
    modify' \ctx -> ctx
        { ctxAvailableRuleRanges = AlignableMap.insert newV
            do Just hr
            do ctxAvailableRuleRanges ctx
        }

getAvailableVar
    :: PEG.VarNum -> Pipeline start varDoc altDoc a (Maybe LAPEG.VarNum)
getAvailableVar v = do
    ctx <- lift get
    case AlignableMap.lookup v do ctxVarMap ctx of
        Nothing ->
            pure Nothing
        Just newV -> case AlignableMap.lookup newV do ctxAvailableRuleRanges ctx of
            Nothing ->
                pure Nothing
            Just Nothing ->
                pure Nothing
            Just Just{} ->
                pure do Just newV

popUpdateRuleItem
    :: Pipeline start varDoc altDoc a (Maybe (LAPEG.VarNum, LAPEG.HeadRange, [PEG.Alt altDoc a]))
popUpdateRuleItem = do
    updateRuleStack <- getCtx ctxUpdateRuleStack
    case updateRuleStack of
        [] ->
            pure Nothing
        item:items -> do
            lift do modify' \ctx -> ctx { ctxUpdateRuleStack = items }
            pure do Just item

pushUpdateRuleItem
    :: LAPEG.VarNum -> LAPEG.HeadRange -> [PEG.Alt altDoc a]
    -> Pipeline start varDoc altDoc a ()
pushUpdateRuleItem newV newRange alts = lift do
    modify' \ctx -> ctx
        { ctxUpdateRuleStack = (newV, newRange, alts):ctxUpdateRuleStack ctx
        }

getCtx
    :: (Context start varDoc altDoc a -> r)
    -> Pipeline start varDoc altDoc a r
getCtx f = f <$> lift get

throwV :: PEG.VarNum -> Pipeline start varDoc altDoc a r
throwV v = throwE do AlignableSet.singleton v

liftBuilder
    :: LAPEGBuilder.T start varDoc altDoc a Identity r
    -> Pipeline start varDoc altDoc a r
liftBuilder builder = do
    ctx <- lift get
    let (x, builderCtx) = runState builder do ctxBuilder ctx
    lift do put do ctx { ctxBuilder = builderCtx }
    pure x
