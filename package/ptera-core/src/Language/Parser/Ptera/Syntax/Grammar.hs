module Language.Parser.Ptera.Syntax.Grammar (
    T,

    GrammarT,
    Context (..),
    fixGrammarT,
    FixedGrammar (..),

    Action (..),
    RuleExpr (..),
    Alt (..),
    Expr (..),
    Unit (..),

    initialT,
    ruleT,
) where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict           as EnumMap


type T start nonTerminal terminal elem doc action =
    GrammarT start nonTerminal terminal elem doc action

type GrammarT start nonTerminal terminal elem doc action =
    StateT (Context start nonTerminal terminal elem doc action)

data Context start nonTerminal terminal elem doc action = Context
    { ctxStarts :: EnumMap.EnumMap start nonTerminal
    , ctxRules  :: EnumMap.EnumMap nonTerminal (RuleExpr nonTerminal terminal elem action)
    , ctxDisplayNonTerminals :: EnumMap.EnumMap nonTerminal doc
    }

fixGrammarT :: Monad m
    => GrammarT start nonTerminal terminal elem doc action m ()
    -> m (FixedGrammar start nonTerminal terminal elem doc action)
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

data FixedGrammar start nonTerminal terminal elem doc action = FixedGrammar
    { grammarStarts :: EnumMap.EnumMap start nonTerminal
    , grammarRules  :: EnumMap.EnumMap nonTerminal (RuleExpr nonTerminal terminal elem action)
    , grammarDisplayNonTerminals :: EnumMap.EnumMap nonTerminal doc
    }

data Action (action :: [Type] -> Type -> Type) where
    Action :: action us a -> Action action

data RuleExpr nonTerminal terminal elem action where
    RuleExpr :: [Alt nonTerminal terminal elem action a] -> RuleExpr nonTerminal terminal elem action

data Alt nonTerminal terminal elem action a where
    Alt :: Expr nonTerminal terminal elem us -> action us a
        -> Alt nonTerminal terminal elem action a

data Expr nonTerminal terminal elem us where
    Eps :: Expr nonTerminal terminal elem '[]
    (:^) :: Unit nonTerminal terminal elem u -> Expr nonTerminal terminal elem us
        -> Expr nonTerminal terminal elem (u ': us)

infixr 5 :^

data Unit nonTerminal terminal elem u where
    UnitToken :: terminal -> Unit nonTerminal terminal elem elem
    UnitVar :: nonTerminal -> Unit nonTerminal terminal elem u

initialT :: Enum start => Monad m => start -> nonTerminal
    -> GrammarT start nonTerminal terminal elem doc action m ()
initialT s v = modify' \ctx -> ctx
    {
        ctxStarts = EnumMap.insert s v
            do ctxStarts ctx
    }

ruleT :: Enum nonTerminal => Monad m
    => nonTerminal -> doc -> RuleExpr nonTerminal terminal elem action
    -> GrammarT start nonTerminal terminal elem doc action m ()
ruleT v d e = modify' \ctx -> ctx
    { ctxRules = EnumMap.insert v e
        do ctxRules ctx
    , ctxDisplayNonTerminals = EnumMap.insert v d
        do ctxDisplayNonTerminals ctx
    }
