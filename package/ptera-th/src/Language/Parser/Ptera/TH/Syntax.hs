{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.Parser.Ptera.TH.Syntax (
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
    TypedExpr,
    SemActM (..),
    semActM,
    HFList.HFList (..),
    HFList.DictF (..),
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
    (<^>),
    eps,
    (<:>),
    (<::>),
    var,
    varA,
    tok,
    SafeGrammar.TokensMember (..),
    tokA,
) where

import           Language.Parser.Ptera.Prelude

import qualified Language.Haskell.TH                      as TH
import qualified Language.Haskell.TH.Syntax               as TH
import qualified Language.Parser.Ptera.Data.HFList        as HFList
import qualified Language.Parser.Ptera.Syntax             as Syntax
import qualified Language.Parser.Ptera.Syntax.Grammar             as SyntaxGrammar
import qualified Language.Parser.Ptera.Syntax.SafeGrammar as SafeGrammar
import           Language.Parser.Ptera.TH.ParserLib
import qualified Language.Parser.Ptera.TH.Class.LiftType as LiftType
import qualified Language.Parser.Ptera.Data.HEnum     as HEnum
import qualified Type.Membership as Membership


type T ctx = GrammarM ctx

type GrammarM ctx = SafeGrammar.Grammar (SemActM ctx)
type RuleExprM ctx = SafeGrammar.RuleExpr (SemActM ctx)
type AltM ctx = SafeGrammar.Alt (SemActM ctx)

type Grammar = GrammarM ()
type RuleExpr = RuleExprM ()
type Alt = AltM ()

data TypedExpr rules tokens elem us = TypedExpr
    { unTypedExpr :: SafeGrammar.Expr rules tokens elem us
    , getTypesOfExpr :: HFList.T TTypeQ us
    }

newtype TTypeQ a = TTypeQ (TH.Q TH.Type)


eps :: TypedExpr rules tokens elem '[]
eps = TypedExpr
    { unTypedExpr = SafeGrammar.UnsafeExpr HFList.HFNil
    , getTypesOfExpr = HFList.HFNil
    }

(<^>)
    :: TypedExpr rules tokens elem us1 -> TypedExpr rules tokens elem us2
    -> TypedExpr rules tokens elem (HFList.Concat us1 us2)
e1 <^> e2 = TypedExpr
    { unTypedExpr = SafeGrammar.UnsafeExpr
        do HFList.hconcat
            do SafeGrammar.unsafeExpr do unTypedExpr e1
            do SafeGrammar.unsafeExpr do unTypedExpr e2
    , getTypesOfExpr = HFList.hconcat
        do getTypesOfExpr e1
        do getTypesOfExpr e2
    }

infixr 5 <^>


(<:>)
    :: TypedExpr rules tokens elem us -> (HTExpList us -> TH.Q (TH.TExp a))
    -> AltM ctx rules tokens elem a
e <:> act = unTypedExpr e SafeGrammar.<:> semAct act do getTypesOfExpr e

infixl 4 <:>

(<::>)
    :: TypedExpr rules tokens elem us
    -> (HTExpList us -> TH.Q (TH.TExp (ActionTask ctx a)))
    -> AltM ctx rules tokens elem a
e <::> act = unTypedExpr e SafeGrammar.<:> semActM act do getTypesOfExpr e

infixl 4 <::>


var :: forall v rules tokens elem proxy1 proxy2 a.
    KnownSymbol v => a ~ SafeGrammar.RuleExprReturnType rules v => LiftType.T a =>
    proxy1 rules -> proxy2 v -> TypedExpr rules tokens elem '[a]
var _ pv = TypedExpr
    { unTypedExpr = SafeGrammar.UnsafeExpr do HFList.HFCons u HFList.HFNil
    , getTypesOfExpr = HFList.HFCons tq HFList.HFNil
    } where
        u = SyntaxGrammar.UnitVar do symbolVal pv
        tq = TTypeQ do LiftType.liftType do Proxy @a

varA :: forall v rules tokens elem a.
    KnownSymbol v => a ~ SafeGrammar.RuleExprReturnType rules v => LiftType.T a =>
    TypedExpr rules tokens elem '[a]
varA = var
    do Proxy @rules
    do Proxy @v

tok :: forall t rules tokens elem proxy. LiftType.T elem
    => proxy elem -> Membership.Membership (SafeGrammar.TokensTag tokens) t
    -> TypedExpr rules tokens elem '[elem]
tok pe pm = TypedExpr
    { unTypedExpr = SafeGrammar.UnsafeExpr do HFList.HFCons u HFList.HFNil
    , getTypesOfExpr = HFList.HFCons tq HFList.HFNil
    } where
        u = SyntaxGrammar.UnitToken
            do HEnum.unsafeHEnum do HEnum.henum pm
        tq = TTypeQ do LiftType.liftType pe

tokA :: forall t rules tokens elem.
    LiftType.T elem => SafeGrammar.TokensMember tokens t =>
    TypedExpr rules tokens elem '[elem]
tokA = tok
    do Proxy @elem
    do SafeGrammar.tokensMembership do proxy# @'(tokens, t)


type HTExpList = HFList.T TExpQ

newtype TExpQ a = TExpQ
    { unTExpQ :: TH.Q (TH.TExp a)
    }

pattern HNil :: HTExpList '[]
pattern HNil = HFList.HFNil

pattern (:*) :: TH.Q (TH.TExp u) -> HTExpList us -> HTExpList (u ': us)
pattern e :* es = HFList.HFCons (TExpQ e) es

infixr 6 :*


type SemActM :: Type -> [Type] -> Type -> Type
newtype SemActM ctx us a = UnsafeSemActM
    { unsafeSemanticAction :: TH.Q TH.Exp
    }

type SemAct = SemActM ()

semActM
    :: (HTExpList us -> TH.Q (TH.TExp (Syntax.ActionTask ctx a)))
    -> HFList.T TTypeQ us -> SemActM ctx us a
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

    actArgs :: HFList.T TTypeQ us -> TH.Q ([TH.Name], HTExpList us)
    actArgs = \case
        HFList.HFNil ->
            pure ([], HNil)
        HFList.HFCons (TTypeQ t) xs -> do
            n <- TH.newName "pteraTHSemActArg"
            let e = pure do TH.VarE n
            let arg = TH.unsafeTExpCoerce
                    [|pteraTHUnsafeExtractReduceArgument $(e) :: $(t)|]
            (ns, args) <- actArgs xs
            pure (n:ns, arg :* args)

semAct
    :: (HTExpList us -> TH.Q (TH.TExp a))
    -> HFList.T TTypeQ us -> SemActM ctx us a
semAct f = semActM do \us -> [||pteraTHActionTaskPure $$(f us)||]
