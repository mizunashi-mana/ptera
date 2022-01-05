module Language.Parser.Ptera.Syntax (
    T,

    HasField (..),
    SafeGrammar.HasRuleExprField (..),
    SafeGrammar.RulesTag,

    GrammarM,
    MemberInitialsM,
    RulesM,
    RuleExprM,
    AltM,
    SafeGrammar.Expr,
    SafeGrammar.Unit,
    GrammarToken.GrammarToken (..),
    SemActM (..),
    ActionTask (..),
    ActionTaskResult (..),
    getAction,
    modifyAction,
    failAction,

    Grammar,
    MemberInitials,
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
type MemberInitialsM ctx = SafeGrammar.MemberInitials (SemActM ctx)
type RulesM ctx = SafeGrammar.Rules (SemActM ctx)
type RuleExprM ctx = SafeGrammar.RuleExpr (SemActM ctx)
type AltM ctx = SafeGrammar.Alt (SemActM ctx)

type Grammar = GrammarM ()
type MemberInitials = MemberInitialsM ()
type Rules = RulesM ()
type RuleExpr = RuleExprM ()
type Alt = AltM ()


newtype SemActM ctx us a = SemActM
    {
        semanticAction :: HList.T us -> ActionTask ctx a
    }
    deriving Functor

type SemAct = SemActM ()

semAct :: (HList.T us -> a) -> SemActM ctx us a
semAct f = SemActM \l -> pure do f l

newtype ActionTask ctx a = ActionTask
    {
        runActionTask :: ctx -> ActionTaskResult ctx a
    }
    deriving Functor

data ActionTaskResult ctx a
    = ActionTaskFail
    | ActionTaskResult a
    | ActionTaskModifyResult ctx a
    deriving (Eq, Show, Functor)

getAction :: ActionTask ctx ctx
getAction = ActionTask \ctx0 -> ActionTaskResult ctx0

modifyAction :: (ctx -> ctx) -> ActionTask ctx ()
modifyAction f = ActionTask \ctx0 -> ActionTaskModifyResult (f ctx0) ()

failAction :: ActionTask ctx a
failAction = ActionTask \_ -> ActionTaskFail

instance Applicative (ActionTask ctx) where
    pure x = ActionTask \_ -> ActionTaskResult x
    ActionTask mf <*> ActionTask mx = ActionTask \ctx0 -> case mf ctx0 of
        ActionTaskFail ->
            ActionTaskFail
        ActionTaskResult f -> case mx ctx0 of
            ActionTaskFail ->
                ActionTaskFail
            ActionTaskResult x ->
                ActionTaskResult do f x
            ActionTaskModifyResult ctx1 x ->
                ActionTaskModifyResult ctx1 do f x
        ActionTaskModifyResult ctx1 f -> case mx ctx1 of
            ActionTaskFail ->
                ActionTaskFail
            ActionTaskResult x ->
                ActionTaskModifyResult ctx1 do f x
            ActionTaskModifyResult ctx2 x ->
                ActionTaskModifyResult ctx2 do f x

instance Monad (ActionTask ctx) where
    ActionTask mx >>= f = ActionTask \ctx0 -> case mx ctx0 of
        ActionTaskFail ->
            ActionTaskFail
        ActionTaskResult x ->
            runActionTask (f x) ctx0
        ActionTaskModifyResult ctx1 x ->
            runActionTask (f x) ctx1
