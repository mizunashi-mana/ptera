module Language.Parser.Ptera.Syntax (
    T,

    GrammarM,
    SafeGrammar.MemberInitials,
    RulesM,
    RuleExprM,
    AltM,
    SafeGrammar.Expr,
    SafeGrammar.Unit,
    GrammarToken.GrammarToken (..),
    SemActM (..),
    ActionTask (..),

    Grammar,
    Rules,
    RuleExpr,
    Alt,
    SemAct,
    semAct,

    SafeGrammar.fixGrammar,
    SafeGrammar.ruleExpr,
    SafeGrammar.alt,
    (SafeGrammar.<^>),
    (SafeGrammar.<:>),
    SafeGrammar.eps,
    SafeGrammar.var,
    SafeGrammar.varA,
    SafeGrammar.tok,
    SafeGrammar.tokA,
) where

import Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Data.HList          as HList
import qualified Language.Parser.Ptera.Syntax.GrammarToken as GrammarToken
import qualified Language.Parser.Ptera.Syntax.SafeGrammar  as SafeGrammar


type T ctx = GrammarM ctx

type GrammarM ctx = SafeGrammar.Grammar (SemActM ctx)
type RulesM ctx rules tokens elem = SafeGrammar.Rules (SemActM ctx) rules tokens elem
type RuleExprM ctx = SafeGrammar.RuleExpr (SemActM ctx)
type AltM ctx = SafeGrammar.Alt (SemActM ctx)

type Grammar = GrammarM ()
type Rules rules tokens elem = RulesM () rules tokens elem
type RuleExpr = RuleExprM ()
type Alt = AltM ()


newtype SemActM ctx us a = SemActM
    {
        semanticAction :: HList.T us -> ActionTask ctx a
    }
    deriving Functor

type SemAct = SemActM ()

semAct :: (HList.T us -> a) -> SemAct us a
semAct f = SemActM \l -> pure do f l

newtype ActionTask ctx a = ActionTask
    {
        runActionTask :: ctx -> (Maybe ctx, a)
    }
    deriving Functor

instance Applicative (ActionTask ctx) where
    pure x = ActionTask \_ -> (Nothing, x)
    ActionTask mf <*> ActionTask mx = ActionTask \ctx0 ->
        let (mctx1, f) = mf ctx0
            ctx1 = case mctx1 of
                Nothing   -> ctx0
                Just ctx  -> ctx
            (mctx2, x) = mx ctx1
        in (mctx2, f x)

instance Monad (ActionTask ctx) where
    ActionTask mx >>= f = ActionTask \ctx0 ->
        let (mctx1, x1) = mx ctx0
            ctx1 = case mctx1 of
                Nothing   -> ctx0
                Just ctx  -> ctx
        in runActionTask
            do f x1
            do ctx1
