module Language.Parser.Ptera.Syntax.Grammar where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                   as EnumMap
import qualified Language.Parser.Ptera.Syntax.SafeRule as SafeRule


type T s n t e f = GrammarT s n t e f

type GrammarT s n t e f = StateT (Context s n t e f)

data Context s n t e f = Context
    {
        ctxStarts :: EnumMap.EnumMap s n,
        ctxRules  :: EnumMap.EnumMap n (RuleWrapper n t e f)
    }

fixed :: Monad m => GrammarT s n t e f m () -> m (FixedGrammar s n t e f)
fixed builder = do
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
        grammarRules  :: EnumMap.EnumMap n (RuleWrapper n t e f)
    }

data Action f where
    Action :: f us r -> Action f

data RuleWrapper n t e f where
    RuleWrapper :: SafeRule.Rule n t e f r -> RuleWrapper n t e f

data Rule n r = Rule n

initialT :: Enum s => Monad m => s -> GrammarT s n t e f m (Rule n r) -> GrammarT s n t e f m ()
initialT s r = do
    Rule v <- r
    modify' \ctx -> ctx
        {
            ctxStarts = EnumMap.insert s v
                do ctxStarts ctx
        }

ruleT :: Enum n => Monad m
    => n -> [GrammarT s n t e f m (SafeRule.Alt n t e f r)]
    -> GrammarT s n t e f m (Rule n r)
ruleT v mes = do
    rules <- ctxRules <$> get
    case EnumMap.lookup v rules of
        Just _ ->
            pure ()
        Nothing -> do
            es <- sequence mes
            let r = RuleWrapper do SafeRule.Rule v es
            modify' \ctx -> ctx
                {
                    ctxRules = EnumMap.insert v r
                        do ctxRules ctx
                }
    pure do Rule v

altT :: Monad m
    => GrammarT s n t e f m (SafeRule.Expr n t e us, f us r)
    -> GrammarT s n t e f m (SafeRule.Alt n t e f r)
altT malt = do
    (e, act) <- malt
    pure do SafeRule.Alt e act

(<^>) :: Monad m
    => GrammarT s n t e f m (SafeRule.Unit n t e u)
    -> GrammarT s n t e f m (SafeRule.Expr n t e us1, f us2 r)
    -> GrammarT s n t e f m (SafeRule.Expr n t e (u ': us1), f us2 r)
mu <^> me = do
    u <- mu
    (e, act) <- me
    pure (u SafeRule.:^ e, act)

infixr 5 <^>

(<:>) :: Monad m
    => GrammarT s n t e f m (SafeRule.Unit n t e u)
    -> f us2 r
    -> GrammarT s n t e f m (SafeRule.Expr n t e '[u], f us2 r)
mu <:> act = do
    u <- mu
    pure (u SafeRule.:^ SafeRule.Eps, act)

infixr 5 <:>

epsT :: Monad m => f '[] r -> GrammarT s n t e f m (SafeRule.Alt n t e f r)
epsT act = pure do SafeRule.Alt SafeRule.Eps act

varT :: Monad m => GrammarT s n t e f m (Rule n r) -> GrammarT s n t e f m (SafeRule.Unit n t e r)
varT mr = do
    Rule v <- mr
    pure do SafeRule.UnitVar v

tokenT :: Monad m => t -> GrammarT s n t e f m (SafeRule.Unit n t e e)
tokenT t = pure do SafeRule.UnitToken t
