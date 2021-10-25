module Language.Parser.Ptera.Pipeline.LAPEG2SRB where

import           Language.Parser.Ptera.Prelude

import qualified Data.Array                                 as Array
import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Alignable.Map as AlignableMap
import qualified Language.Parser.Ptera.Data.Symbolic.IntMap as SymbolicIntMap
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
        ctxVarMap  :: AlignableMap.T LAPEG.Var (SymbolicIntMap.T SRB.StateNum),
        ctxStateMap :: HashMap.HashMap (LAPEG.Position, NonEmpty LAPEG.AltNum) SRB.StateNum,
        ctxStateQueue :: [(SRB.StateNum, LAPEG.Position, NonEmpty LAPEG.AltNum)]
    }

laPegRulePipeline :: LAPEG.Var -> LAPEG.LAPE a -> Pipeline a ()
laPegRulePipeline = undefined

laPegRuleHeadPipeline :: LAPEG.Var -> LAPEG.LAPE a -> Pipeline a (SymbolicIntMap.T SRB.StateNum)
laPegRuleHeadPipeline = undefined

laPegExprPipeline :: LAPEG.Var -> LAPEG.LAPE a
    -> Pipeline a (SymbolicIntMap.T (NonEmpty (SRB.RuleAltNum, [LAPEG.Unit])))
laPegExprPipeline v (LAPEG.LAPE altMap) = traverse
    do \alts -> forM alts \alt -> do
        let newAlt = laPegAltToSrbAlt v alt
        newAltNum <- liftBuilder do SRBBuilder.addRuleAlt newAlt
        pure (newAltNum, LAPEG.altUnitSeq alt)
    do altMap

laPegAltToSrbAlt :: LAPEG.Var -> LAPEG.Alt a -> SRB.RuleAlt a
laPegAltToSrbAlt v alt = SRB.RuleAlt
    {
        ruleAltVar = v,
        ruleAltKind = LAPEG.altKind alt,
        ruleAltAction = LAPEG.altAction alt,
        ruleAltSeq =
            let us = LAPEG.altUnitSeq alt
            in Array.listArray (0, length us - 1) us
    }

liftBuilder :: SRBBuilder.T a Identity r -> Pipeline a r
liftBuilder builder = do
    ctx <- get
    let (x, builderCtx) = runIdentity
            do runStateT builder do ctxBuilder ctx
    put do ctx { ctxBuilder = builderCtx }
    pure x
