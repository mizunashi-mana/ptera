module Language.Parser.Ptera.Syntax.Grammar (
    T,

    GrammarT,
    Context (..),
    fixGrammarT,
    FixedGrammar (..),

    Action (..),
    RuleExpr (..),
    Alt (..),
    Expr,
    Unit (..),

    initialT,
    ruleT,
) where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict           as EnumMap
import qualified Language.Parser.Ptera.Data.HFList as HFList


type T start nonTerminal terminal elem varDoc altDoc action =
    GrammarT start nonTerminal terminal elem varDoc altDoc action

type GrammarT start nonTerminal terminal elem varDoc altDoc action =
    StateT (Context start nonTerminal terminal elem varDoc altDoc action)

data Context start nonTerminal terminal elem varDoc altDoc action = Context
    { ctxStarts :: EnumMap.EnumMap start nonTerminal
    , ctxRules  :: EnumMap.EnumMap nonTerminal (RuleExpr nonTerminal terminal elem altDoc action)
    , ctxDisplayNonTerminals :: EnumMap.EnumMap nonTerminal varDoc
    }

fixGrammarT :: Monad m
    => GrammarT start nonTerminal terminal elem varDoc altDoc action m ()
    -> m (FixedGrammar start nonTerminal terminal elem varDoc altDoc action)
fixGrammarT builder = do
        finalCtx <- execStateT builder initialCtx
        pure do fromCtx finalCtx
    where
        initialCtx = Context
            { ctxStarts = EnumMap.empty
            , ctxRules = EnumMap.empty
            , ctxDisplayNonTerminals = EnumMap.empty
            }

        fromCtx ctx = FixedGrammar
            { grammarStarts = ctxStarts ctx
            , grammarRules = ctxRules ctx
            , grammarDisplayNonTerminals = ctxDisplayNonTerminals ctx
            }

data FixedGrammar start nonTerminal terminal elem varDoc altDoc action = FixedGrammar
    { grammarStarts :: EnumMap.EnumMap start nonTerminal
    , grammarRules  :: EnumMap.EnumMap nonTerminal (RuleExpr nonTerminal terminal elem altDoc action)
    , grammarDisplayNonTerminals :: EnumMap.EnumMap nonTerminal varDoc
    }

data Action (action :: [Type] -> Type -> Type) where
    Action :: action us a -> Action action

data RuleExpr nonTerminal terminal elem altDoc action where
    RuleExpr
        :: [Alt nonTerminal terminal elem altDoc action a]
        -> RuleExpr nonTerminal terminal elem altDoc action

data Alt nonTerminal terminal elem altDoc action a where
    Alt :: Expr nonTerminal terminal elem us -> altDoc -> action us a
        -> Alt nonTerminal terminal elem altDoc action a

type Expr nonTerminal terminal elem = HFList.T (Unit nonTerminal terminal elem)

data Unit nonTerminal terminal elem u where
    UnitToken :: terminal -> Unit nonTerminal terminal elem elem
    UnitVar :: nonTerminal -> Unit nonTerminal terminal elem u

initialT :: Enum start => Monad m
    => start -> nonTerminal
    -> GrammarT start nonTerminal terminal elem varDoc altDoc action m ()
initialT s v = modify' \ctx -> ctx
    {
        ctxStarts = EnumMap.insert s v
            do ctxStarts ctx
    }

ruleT :: Enum nonTerminal => Monad m
    => nonTerminal -> varDoc -> RuleExpr nonTerminal terminal elem altDoc action
    -> GrammarT start nonTerminal terminal elem varDoc altDoc action m ()
ruleT v d e = modify' \ctx -> ctx
    { ctxRules = EnumMap.insert v e
        do ctxRules ctx
    , ctxDisplayNonTerminals = EnumMap.insert v d
        do ctxDisplayNonTerminals ctx
    }
