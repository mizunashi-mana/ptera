module Language.Parser.Ptera.Pipeline.PEG2SSRB where

import           Language.Parser.Ptera.Prelude

import qualified Data.Array                                 as Array
import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Machine.PEG          as PEG
import qualified Language.Parser.Ptera.Machine.SSRB         as SSRB
import qualified Language.Parser.Ptera.Machine.SSRB.Builder as SSRBBuilder

peg2Ssrb :: PEG.T a -> SSRB.T a
peg2Ssrb g = SSRBBuilder.build do
        modify' \builderCtx0 ->
            let initialCtx = Context
                    {
                        ctxVarMap = EnumMap.empty,
                        ctxBuilder = builderCtx0
                    }
                finalCtx = execState pipeline initialCtx
            in ctxBuilder finalCtx
    where
        pipeline = do
            forM (AlignableArray.assocs do PEG.pegRules g) \(v, r) ->
                pegRulePipeline v r
            forM (EnumMap.assocs do PEG.pegInitials g) \(i, v) ->
                registerInitial i v

type Pipeline a = State (Context a)

data Context a = Context
    {
        ctxVarMap  :: EnumMap.EnumMap PEG.Var SSRB.StateNum,
        ctxBuilder :: SSRBBuilder.BuilderContext a
    }

pegRulePipeline :: PEG.Var -> PEG.PE a -> Pipeline a ()
pegRulePipeline v (PEG.PE (alt0 :| alts)) = do
    sn0 <- getStateNumForVar v
    go sn0 alt0 alts
    where
        go sn0 alt = \case
            [] -> do
                let alt' = pegRuleAlt False v alt
                rn <- liftBuilder do SSRBBuilder.addRuleAlt alt'
                pegRuleAltPipeline v sn0 rn alt'
            nalt:alts -> do
                let alt' = pegRuleAlt True v alt
                rn <- liftBuilder do SSRBBuilder.addRuleAlt alt'
                sn1 <- liftBuilder SSRBBuilder.getNewStateNum
                backSn <- liftBuilder SSRBBuilder.getNewStateNum
                addState do backState sn0 sn1 backSn rn
                pegRuleAltPipeline v sn1 rn alt'
                go backSn nalt alts

        backState fromSn toSn backSn rn = SSRB.MState
            { stateNum = fromSn
            , stateTrans = SSRB.TransPushBackpoint backSn toSn
            , stateRuleItem = SSRB.RuleItem
                {
                    ruleItemAltNum = rn,
                    ruleItemCurPos = 0
                }
            }

pegRuleAltPipeline :: PEG.Var -> SSRB.StateNum -> SSRB.RuleAltNum -> SSRB.RuleAlt a -> Pipeline a ()
pegRuleAltPipeline v sn rn alt = case SSRB.ruleAltKind alt of
    PEG.AltNot -> goNot sn
    PEG.AltSeq -> go sn 0
    PEG.AltAnd -> go sn 0
    where
        l = case Array.bounds do SSRB.ruleAltSeq alt of
            (_, ub) -> ub + 1

        go sn0 i = if i >= l
            then
                addState do reduceState sn0 i
            else do
                let u = SSRB.ruleAltSeq alt Array.! i
                case u of
                    PEG.UnitTerminal t -> do
                        sn1 <- liftBuilder SSRBBuilder.getNewStateNum
                        addState do shiftState sn0 t sn1 i
                        go sn1 do i + 1
                    PEG.UnitNonTerminal v -> do
                        sn1 <- liftBuilder SSRBBuilder.getNewStateNum
                        enterSn <- liftBuilder SSRBBuilder.getNewStateNum
                        addState do enterState sn0 v enterSn sn1 i
                        go sn1 do i + 1

        goNot sn0 = do
            backSn <- liftBuilder SSRBBuilder.getNewStateNum
            sn1 <- liftBuilder SSRBBuilder.getNewStateNum
            addState do backState sn0 backSn sn1 0
            addState do reduceNotState backSn 0
            go sn1 0

        shiftState fromSn t toSn i = SSRB.MState
            { stateNum = fromSn
            , stateTrans = SSRB.TransShift
                do fromEnum t
                do toSn
            , stateRuleItem = SSRB.RuleItem
                {
                    ruleItemAltNum = rn,
                    ruleItemCurPos = i
                }
            }

        enterState fromSn v enterSn toSn i = SSRB.MState
            { stateNum = fromSn
            , stateTrans = SSRB.TransEnter v enterSn toSn
            , stateRuleItem = SSRB.RuleItem
                {
                    ruleItemAltNum = rn,
                    ruleItemCurPos = i
                }
            }

        backState fromSn toSn backSn i = SSRB.MState
            { stateNum = fromSn
            , stateTrans = SSRB.TransPushBackpoint backSn toSn
            , stateRuleItem = SSRB.RuleItem
                {
                    ruleItemAltNum = rn,
                    ruleItemCurPos = i
                }
            }

        reduceState fromSn i = SSRB.MState
            { stateNum = fromSn
            , stateTrans = SSRB.TransReduce rn
            , stateRuleItem = SSRB.RuleItem
                {
                    ruleItemAltNum = rn,
                    ruleItemCurPos = i
                }
            }

        reduceNotState fromSn i = SSRB.MState
            { stateNum = fromSn
            , stateTrans = SSRB.TransReduceNot rn
            , stateRuleItem = SSRB.RuleItem
                {
                    ruleItemAltNum = rn,
                    ruleItemCurPos = i
                }
            }

pegRuleAlt :: Bool -> PEG.Var -> PEG.Alt a -> SSRB.RuleAlt a
pegRuleAlt withBack v alt = SSRB.RuleAlt
    { ruleAltVar = v
    , ruleAltSeq = Array.listArray (0, length us - 1) us
    , ruleAltKind = PEG.altKind alt
    , ruleWithBackpoint = withBack
    , ruleAltAction = PEG.altAction alt
    }
    where
        us = PEG.altUnitSeq alt

getStateNumForVar :: PEG.Var -> Pipeline a SSRB.StateNum
getStateNumForVar v = do
    ctx <- get
    let m = ctxVarMap ctx
    case EnumMap.lookup v m of
        Just sn ->
            pure sn
        Nothing -> do
            sn <- liftBuilder do SSRBBuilder.getNewStateNum
            put do ctx { ctxVarMap = EnumMap.insert v sn m }
            pure sn

registerInitial :: PEG.StartPoint -> PEG.Var -> Pipeline a ()
registerInitial i v = do
    m <- ctxVarMap <$> get
    case EnumMap.lookup v m of
        Just sn -> liftBuilder do SSRBBuilder.registerInitial i sn
        Nothing -> pure ()

addState :: SSRB.MState -> Pipeline a ()
addState s = liftBuilder do SSRBBuilder.addState s

liftBuilder :: SSRBBuilder.T a r -> Pipeline a r
liftBuilder builder = do
    ctx <- get
    let (x, builderCtx) = runState builder do ctxBuilder ctx
    put do ctx { ctxBuilder = builderCtx }
    pure x
