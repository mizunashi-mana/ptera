module Language.Parser.Ptera.Pipeline.Grammar2PEG where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Syntax.Grammar as Grammar
import qualified Language.Parser.Ptera.Machine.PEG as PEG
import qualified Language.Parser.Ptera.Machine.PEG.Builder as PEGBuilder
import qualified Language.Parser.Ptera.Syntax.SafeRule as SafeRule
import qualified Data.EnumMap.Strict as EnumMap


grammar2Peg :: Enum s => Enum n => Enum t
    => Grammar.FixedGrammar s n t e f -> PEG.T s (Action f)
grammar2Peg g = runIdentity do PEGBuilder.build builder where
    builder = do
        initialBuilderCtx <- get
        let initialCtx = Context
                {
                    ctxBuilder = initialBuilderCtx,
                    ctxVarMap = EnumMap.empty
                }
        let finalCtx = execState pipeline initialCtx
        put do ctxBuilder finalCtx

    pipeline = do
        forM_ do EnumMap.assocs do Grammar.grammarStarts g
            do \(s, v) -> grammarStartPipeline s v
        forM_ do EnumMap.assocs do Grammar.grammarRules g
            do \(_, r) -> grammarRulePipeline r

data Action f where
    Action :: f us r -> Action f

type Pipeline s n f = State (Context s n f)

data Context s n f = Context
    {
        ctxBuilder        :: PEGBuilder.Context s (Action f),
        ctxVarMap         :: EnumMap.EnumMap n PEG.Var
    }

grammarStartPipeline :: Enum s => Enum n => s -> n -> Pipeline s n f ()
grammarStartPipeline s v = do
    newV <- getNewVar v
    liftBuilder do PEGBuilder.addInitial s newV

grammarRulePipeline :: Enum n => Enum t
    => Grammar.RuleWrapper n t e f -> Pipeline s n f ()
grammarRulePipeline (Grammar.RuleWrapper (SafeRule.Rule v alts)) = do
    newV <- getNewVar v
    newAlts <- forM alts \alt -> grammarAltPipeline alt
    let newRule = PEG.Rule newAlts
    liftBuilder do PEGBuilder.addRule newV newRule

grammarAltPipeline :: Enum n => Enum t
    => SafeRule.Alt n t e f r -> Pipeline s n f (PEG.Alt (Action f))
grammarAltPipeline (SafeRule.Alt e act) = do
    newUs <- grammarExprPipeline e
    let newAct = Action act
    let newAlt = PEG.Alt
            {
                altKind = PEG.AltSeq,
                altUnitSeq = newUs,
                altAction = newAct
            }
    pure newAlt

grammarExprPipeline :: forall s n t e f us. Enum n => Enum t
    => SafeRule.Expr n t e us -> Pipeline s n f [PEG.Unit]
grammarExprPipeline = \e -> go [] e where
    go :: [PEG.Unit] -> SafeRule.Expr n t e us' -> Pipeline s n f [PEG.Unit]
    go acc = \case
        SafeRule.Eps ->
            pure do reverse acc
        u SafeRule.:^ e -> do
            newU <- grammarUnitPipeline u
            go
                do newU:acc
                do e

grammarUnitPipeline :: Enum n => Enum t
    => SafeRule.Unit n t e u -> Pipeline s n f PEG.Unit
grammarUnitPipeline = \case
    SafeRule.UnitToken t ->
        pure do PEG.UnitTerminal do fromEnum t
    SafeRule.UnitVar v -> do
        newV <- getNewVar v
        pure do PEG.UnitNonTerminal newV

getNewVar :: Enum n => n -> Pipeline s n f PEG.Var
getNewVar v = do
    vmap <- ctxVarMap <$> get
    case EnumMap.lookup v vmap of
        Just newV ->
            pure newV
        Nothing -> do
            newV <- liftBuilder PEGBuilder.genNewVar
            modify' \ctx -> ctx
                {
                    ctxVarMap = EnumMap.insert v newV
                        do ctxVarMap ctx
                }
            pure newV

liftBuilder :: PEGBuilder.T s (Action f) Identity r -> Pipeline s n f r
liftBuilder builder = do
    ctx <- get
    let (x, builderCtx) = runState builder do ctxBuilder ctx
    put do ctx { ctxBuilder = builderCtx }
    pure x
