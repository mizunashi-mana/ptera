{-# LANGUAGE TemplateHaskell #-}

module Language.Parser.Ptera.TH.Syntax (
    T,

    HasField (..),
    SafeGrammar.HasRuleExprField (..),
    SafeGrammar.TokensTag,
    SafeGrammar.RulesTag,
    SafeGrammar.RuleExprType,

    GrammarM,
    SafeGrammar.MemberInitials,
    SafeGrammar.Rules,
    SafeGrammar.GrammarToken (..),
    RuleExprM,
    AltM,
    SafeGrammar.Expr,
    SemActM (..),
    semActM,
    HTExpList,
    pattern HNil,
    pattern (:*),
    TExpQ (..),
    Syntax.ActionTask (..),
    Syntax.ActionTaskResult (..),
    Syntax.getAction,
    Syntax.modifyAction,
    Syntax.failAction,

    Grammar,
    RuleExpr,
    Alt,
    SemAct,
    semAct,

    SafeGrammar.fixGrammar,
    SafeGrammar.ruleExpr,
    (SafeGrammar.<^>),
    (<:>),
    eps,
    (<::>),
    epsM,
    SafeGrammar.var,
    SafeGrammar.varA,
    SafeGrammar.tok,
    SafeGrammar.TokensMember (..),
    SafeGrammar.tokA,
) where

import           Language.Parser.Ptera.Prelude

import qualified Language.Haskell.TH                      as TH
import qualified Language.Haskell.TH.Syntax               as TH
import qualified Language.Parser.Ptera.Syntax             as Syntax
import qualified Language.Parser.Ptera.Syntax.SafeGrammar as SafeGrammar
import           Language.Parser.Ptera.TH.ParserLib
import qualified Type.Membership.HList as Membership


type T ctx = GrammarM ctx

type GrammarM ctx = SafeGrammar.Grammar (SemActM ctx)
type RuleExprM ctx = SafeGrammar.RuleExpr (SemActM ctx)
type AltM ctx = SafeGrammar.Alt (SemActM ctx)

type Grammar = GrammarM ()
type RuleExpr = RuleExprM ()
type Alt = AltM ()


(<:>)
    :: SafeGrammar.Expr rules tokens elem us -> (HTExpList us -> TH.Q (TH.TExp a))
    -> AltM ctx rules tokens elem a
e@(SafeGrammar.UnsafeExpr ue) <:> act = e SafeGrammar.<:> semAct act ue

infixl 4 <:>

eps :: (HTExpList '[] -> TH.Q (TH.TExp a)) -> AltM ctx rules tokens elem a
eps act = SafeGrammar.eps do semAct act Membership.HNil

(<::>)
    :: SafeGrammar.Expr rules tokens elem us
    -> (HTExpList us -> TH.Q (TH.TExp (ActionTask ctx a)))
    -> AltM ctx rules tokens elem a
e@(SafeGrammar.UnsafeExpr ue) <::> act = e SafeGrammar.<:> semActM act ue

infixl 4 <::>

epsM
    :: (HTExpList '[] -> TH.Q (TH.TExp (ActionTask ctx a)))
    -> AltM ctx rules tokens elem a
epsM act = SafeGrammar.eps do semActM act Membership.HNil


type HTExpList = Membership.HList TExpQ

newtype TExpQ a = TExpQ
    { unTExpQ :: TH.Q (TH.TExp a)
    }

pattern HNil :: HTExpList '[]
pattern HNil = Membership.HNil

pattern (:*) :: TH.Q (TH.TExp u) -> HTExpList us -> HTExpList (u ': us)
pattern e :* es = Membership.HCons (TExpQ e) es

infixr 6 :*


type SemActM :: Type -> [Type] -> Type -> Type
newtype SemActM ctx us a = UnsafeSemActM
    { unsafeSemanticAction :: TH.Q TH.Exp
    }

type SemAct = SemActM ()

semActM
    :: (HTExpList us -> TH.Q (TH.TExp (Syntax.ActionTask ctx a)))
    -> Membership.HList f us -> SemActM ctx us a
semActM f xs0 = UnsafeSemActM go where
    go = do
        (ns, args) <- actArgs xs0
        l <- TH.newName "pteraTHSemActArgs"
        let lp = pure do TH.VarP l
        let le = pure do TH.VarE l
        let lp0 = pure do TH.ListP [TH.VarP n | n <- ns]
        [e|\ $(lp) -> case $(le) of
            $(lp0) ->
                $(TH.unType <$> f args)
            _ ->
                error "unreachable: unexpected arguments"
            |]

    actArgs :: Membership.HList f us -> TH.Q ([TH.Name], HTExpList us)
    actArgs = \case
        Membership.HNil ->
            pure ([], HNil)
        Membership.HCons _ xs -> do
            n <- TH.newName "pteraTHSemActArg"
            let ne = TH.unsafeTExpCoerce do pure do TH.VarE n
            let arg = [||pteraTHUnsafeExtractReduceArgument $$(ne)||]
            (ns, args) <- actArgs xs
            pure (n:ns, arg :* args)

semAct
    :: (HTExpList us -> TH.Q (TH.TExp a))
    -> Membership.HList f us -> SemActM ctx us a
semAct f = semActM do \us -> [||pteraTHActionTaskPure $$(f us)||]
