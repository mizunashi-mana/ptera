module Language.Parser.Ptera.Pipeline.Grammar2PEG where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                       as EnumMap
import qualified Language.Parser.Ptera.Machine.PEG         as PEG
import qualified Language.Parser.Ptera.Machine.PEG.Builder as PEGBuilder
import qualified Language.Parser.Ptera.Syntax.Grammar      as Grammar
import qualified Language.Parser.Ptera.Data.HFList as HFList


grammar2Peg :: Enum start => Enum nonTerminal => Enum terminal
    => Grammar.FixedGrammar start nonTerminal terminal elem varDoc altDoc action
    -> PEG.T start varDoc altDoc (Grammar.Action action)
grammar2Peg g = runIdentity do PEGBuilder.build builder where
    builder = do
        initialBuilderCtx <- get
        let initialCtx = Context
                { ctxBuilder = initialBuilderCtx
                , ctxVarMap = EnumMap.empty
                , ctxDisplayNonTerminals = Grammar.grammarDisplayNonTerminals g
                }
        let finalCtx = execState pipeline initialCtx
        put do ctxBuilder finalCtx

    pipeline = do
        forM_ do EnumMap.assocs do Grammar.grammarStarts g
            do \(s, v) -> grammarStartPipeline s v
        forM_ do EnumMap.assocs do Grammar.grammarRules g
            do \(v, e) -> grammarRulePipeline v e

type Pipeline start nonTerminal varDoc altDoc action =
    State (Context start nonTerminal varDoc altDoc action)

data Context start nonTerminal varDoc altDoc action = Context
    { ctxBuilder :: PEGBuilder.Context start varDoc altDoc (Grammar.Action action)
    , ctxVarMap  :: EnumMap.EnumMap nonTerminal PEG.VarNum
    , ctxDisplayNonTerminals :: EnumMap.EnumMap nonTerminal varDoc
    }

grammarStartPipeline :: Enum start => Enum nonTerminal
    => start -> nonTerminal -> Pipeline start nonTerminal varDoc altDoc action ()
grammarStartPipeline s v = do
    newV <- getNewVar v
    liftBuilder do PEGBuilder.addInitial s newV

grammarRulePipeline :: Enum nonTerminal => Enum terminal
    => nonTerminal -> Grammar.RuleExpr nonTerminal terminal elem altDoc action
    -> Pipeline start nonTerminal varDoc altDoc action ()
grammarRulePipeline v (Grammar.RuleExpr alts) = do
    newV <- getNewVar v
    newAlts <- forM alts \alt -> grammarAltPipeline alt
    let newRule = PEG.Rule newAlts
    liftBuilder do PEGBuilder.addRule newV newRule

grammarAltPipeline :: Enum nonTerminal => Enum terminal
    => Grammar.Alt nonTerminal terminal elem altDoc action r
    -> Pipeline start nonTerminal varDoc altDoc action PEG.AltNum
grammarAltPipeline (Grammar.Alt e d act) = do
    newUs <- grammarExprPipeline e
    let newAct = Grammar.Action act
    let newAlt = PEG.Alt
            { altKind = PEG.AltSeq
            , altUnitSeq = newUs
            , altAction = newAct
            , altHelp = d
            }
    liftBuilder do PEGBuilder.genNewAlt newAlt

grammarExprPipeline :: forall start nonTerminal terminal elem varDoc altDoc action us
    .  Enum nonTerminal => Enum terminal
    => Grammar.Expr nonTerminal terminal elem us
    -> Pipeline start nonTerminal varDoc altDoc action [PEG.Unit]
grammarExprPipeline e = do
    revUs <- HFList.hfoldMWithIndex []
        do \acc _ u -> do
            newU <- grammarUnitPipeline u
            pure do newU:acc
        do e
    pure do reverse revUs

grammarUnitPipeline :: Enum nonTerminal => Enum terminal
    => Grammar.Unit nonTerminal terminal elem u
    -> Pipeline start nonTerminal varDoc altDoc action PEG.Unit
grammarUnitPipeline = \case
    Grammar.UnitToken t ->
        pure do PEG.UnitTerminal do fromEnum t
    Grammar.UnitVar v -> do
        newV <- getNewVar v
        pure do PEG.UnitNonTerminal newV

getNewVar :: Enum nonTerminal
    => nonTerminal -> Pipeline start nonTerminal varDoc altDoc action PEG.VarNum
getNewVar v = do
    vmap <- ctxVarMap <$> get
    case EnumMap.lookup v vmap of
        Just newV ->
            pure newV
        Nothing -> do
            displayNonTerminals <- ctxDisplayNonTerminals <$> get
            let d = case EnumMap.lookup v displayNonTerminals of
                    Just x  -> x
                    Nothing -> error "Not found any rules for a non-terminal."
            newV <- liftBuilder
                do PEGBuilder.genNewVar
                    do PEG.Var
                        { varHelp = d
                        }
            modify' \ctx -> ctx
                { ctxVarMap = EnumMap.insert v newV
                    do ctxVarMap ctx
                }
            pure newV

liftBuilder
    :: PEGBuilder.T start varDoc altDoc (Grammar.Action action) Identity r
    -> Pipeline start nonTerminal varDoc altDoc action r
liftBuilder builder = do
    ctx <- get
    let (x, builderCtx) = runState builder do ctxBuilder ctx
    put do ctx { ctxBuilder = builderCtx }
    pure x
