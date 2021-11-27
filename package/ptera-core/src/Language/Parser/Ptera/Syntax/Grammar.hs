module Language.Parser.Ptera.Syntax.Grammar where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict           as EnumMap


type T s n t e f = GrammarT s n t e f

type GrammarT s n t e f = StateT (Context s n t e f)

data Context s n t e f = Context
    {
        ctxStarts :: EnumMap.EnumMap s n,
        ctxRules  :: EnumMap.EnumMap n (RuleExpr n t e f)
    }

fixGrammarT :: Monad m => GrammarT s n t e f m () -> m (FixedGrammar s n t e f)
fixGrammarT builder = do
        finalCtx <- execStateT builder initialCtx
        pure do fromCtx finalCtx
    where
        initialCtx = Context
            {
                ctxStarts = EnumMap.empty,
                ctxRules = EnumMap.empty
            }

        fromCtx ctx = FixedGrammar
            {
                grammarStarts = ctxStarts ctx,
                grammarRules = ctxRules ctx
            }

data FixedGrammar s n t e f = FixedGrammar
    {
        grammarStarts :: EnumMap.EnumMap s n,
        grammarRules  :: EnumMap.EnumMap n (RuleExpr n t e f)
    }

data Action (f :: [Type] -> Type -> Type) where
    Action :: f us r -> Action f

data RuleExpr n t e f where
    RuleExpr :: [Alt n t e f r] -> RuleExpr n t e f

data Alt n t e f r where
    Alt :: Expr n t e us -> f us r -> Alt n t e f r

data Expr n t e us where
    Eps :: Expr n t e '[]
    (:^) :: Unit n t e u -> Expr n t e us -> Expr n t e (u ': us)

infixr 5 :^

data Unit n t e u where
    UnitToken :: t -> Unit n t e e
    UnitVar :: n -> Unit n t e u

initialT :: Enum s => Monad m => s -> n -> GrammarT s n t e f m ()
initialT s v = modify' \ctx -> ctx
    {
        ctxStarts = EnumMap.insert s v
            do ctxStarts ctx
    }

ruleT :: Enum n => Monad m => n -> RuleExpr n t e f -> GrammarT s n t e f m ()
ruleT v e = modify' \ctx -> ctx
    {
        ctxRules = EnumMap.insert v e
            do ctxRules ctx
    }
