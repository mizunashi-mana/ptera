module Language.Parser.Ptera.Syntax (
    T,

    SafeGrammar.HasRuleExprField (..),
    SafeGrammar.TokensTag,
    SafeGrammar.RulesTag,
    SafeGrammar.RuleExprType,

    GrammarM,
    SafeGrammar.MemberInitials (..),
    SafeGrammar.Rules (..),
    SafeGrammar.GrammarToken (..),
    RuleExprM,
    AltM,
    SafeGrammar.Expr,
    HFList.HFList (..),
    HFList.DictF (..),
    HList,
    pattern HNil,
    pattern (:*),
    SemActM (..),
    semActM,
    ActionTask (..),
    ActionTaskResult (..),
    getAction,
    modifyAction,
    failAction,

    Grammar,
    RuleExpr,
    Alt,
    SemAct,
    semAct,

    SafeGrammar.fixGrammar,
    SafeGrammar.ruleExpr,
    (SafeGrammar.<^>),
    SafeGrammar.eps,
    (<:>),
    (<::>),
    SafeGrammar.var,
    SafeGrammar.varA,
    SafeGrammar.tok,
    SafeGrammar.TokensMember (..),
    SafeGrammar.tokA,
) where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Data.HFList        as HFList
import qualified Language.Parser.Ptera.Syntax.SafeGrammar as SafeGrammar


type T ctx = GrammarM ctx

type GrammarM ctx = SafeGrammar.Grammar (SemActM ctx)
type RuleExprM ctx = SafeGrammar.RuleExpr (SemActM ctx)
type AltM ctx = SafeGrammar.Alt (SemActM ctx)

type Grammar = GrammarM ()
type RuleExpr = RuleExprM ()
type Alt = AltM ()


(<:>)
    :: SafeGrammar.Expr rules tokens elem us -> (HList us -> a)
    -> AltM ctx rules tokens elem a
e <:> act = e SafeGrammar.<:> semAct act

infixl 4 <:>

(<::>)
    :: SafeGrammar.Expr rules tokens elem us -> (HList us -> ActionTask ctx a)
    -> AltM ctx rules tokens elem a
e <::> act = e SafeGrammar.<:> semActM act

infixl 4 <::>


type HList = HFList.T Identity

pattern HNil :: HList '[]
pattern HNil = HFList.HFNil

pattern (:*) :: u -> HList us -> HList (u ': us)
pattern x :* xs = HFList.HFCons (Identity x) xs

infixr 6 :*


newtype SemActM ctx us a = SemActM
    { semanticAction :: HList us -> ActionTask ctx a
    }
    deriving Functor

type SemAct = SemActM ()

semActM :: (HList us -> ActionTask ctx a) -> SemActM ctx us a
semActM = SemActM

semAct :: (HList us -> a) -> SemActM ctx us a
semAct f = SemActM \l -> pure do f l


newtype ActionTask ctx a = ActionTask
    { runActionTask :: ctx -> ActionTaskResult ctx a
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
