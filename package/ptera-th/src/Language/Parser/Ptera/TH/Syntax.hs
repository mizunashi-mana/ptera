{-# LANGUAGE TemplateHaskell #-}

module Language.Parser.Ptera.TH.Syntax (
    T,

    HasField (..),
    SafeGrammar.HasRuleExprField (..),
    SafeGrammar.RulesTag,
    SafeGrammar.RuleExprType,

    GrammarM,
    SafeGrammar.MemberInitials,
    SafeGrammar.Rules,
    RuleExprM,
    AltM,
    SafeGrammar.Expr,
    SafeGrammar.Unit,
    GrammarToken.GrammarToken (..),
    SemActM (..),
    semActM,
    semActM',
    semActM_,
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
    semAct',
    semAct_,

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

import           Language.Parser.Ptera.Prelude

import qualified Language.Haskell.TH                       as TH
import qualified Language.Haskell.TH.Syntax                as TH
import qualified Language.Parser.Ptera.Data.HList          as HList
import qualified Language.Parser.Ptera.Syntax as Syntax
import qualified Language.Parser.Ptera.Syntax.GrammarToken as GrammarToken
import qualified Language.Parser.Ptera.Syntax.SafeGrammar  as SafeGrammar
import           Language.Parser.Ptera.TH.ParserLib


type T ctx = GrammarM ctx

type GrammarM ctx = SafeGrammar.Grammar (SemActM ctx)
type RuleExprM ctx = SafeGrammar.RuleExpr (SemActM ctx)
type AltM ctx = SafeGrammar.Alt (SemActM ctx)

type Grammar = GrammarM ()
type RuleExpr = RuleExprM ()
type Alt = AltM ()

type SemActM :: Type -> [Type] -> Type -> Type
newtype SemActM ctx us a = UnsafeSemActM
    {
        unsafeSemanticAction :: TH.Q TH.Exp
    }

type SemAct = SemActM ()

semActM :: forall ctx us a. SemActArgs us
    => (HList.T (TypedActArgs us) -> TH.Q (TH.TExp (Syntax.ActionTask ctx a))) -> SemActM ctx us a
semActM f = semActM_ \us -> [e|$(TH.unType <$> f us)|]

semAct :: forall ctx us a. SemActArgs us
    => (HList.T (TypedActArgs us) -> TH.Q (TH.TExp a)) -> SemActM ctx us a
semAct f = semActM_ \us -> [e|pure $(TH.unType <$> f us)|]

semActM' :: forall ctx us a. SemActArgs us
    => (HList.T (UntypedActArgs us) -> TH.Q TH.Exp) -> SemActM ctx us a
semActM' f = semActM_ \us -> [e|$(f do unTypeArgs @us proxy# us)|]

semAct' :: forall ctx us a. SemActArgs us
    => (HList.T (UntypedActArgs us) -> TH.Q TH.Exp) -> SemActM ctx us a
semAct' f = semActM_ \us -> [e|pure $(f do unTypeArgs @us proxy# us)|]

semActM_ :: forall ctx us a. SemActArgs us
    => (HList.T (TypedActArgs us) -> TH.Q TH.Exp) -> SemActM ctx us a
semActM_ f = UnsafeSemActM do
    (ns, args) <- unsafeSemActArgs do proxy# :: Proxy# us
    l <- TH.newName "pteraTHSemActArgs"
    let lp = pure do TH.VarP l
    let le = pure do TH.VarE l
    let lp0 = pure do TH.ListP [TH.VarP n | n <- ns]
    [e|\ $(lp) -> case $(le) of
        $(lp0) ->
            $(f args)
        _ ->
            error "unreachable: unexpected arguments"
        |]

semAct_ :: forall ctx us a. SemActArgs us
    => (HList.T (TypedActArgs us) -> TH.Q TH.Exp) -> SemActM ctx us a
semAct_ f = semActM_ \us -> [e|pure $(f us)|]

class SemActArgs (us :: [Type]) where
    type TypedActArgs us :: [Type]
    type UntypedActArgs us :: [Type]

    unsafeSemActArgs :: Proxy# us -> TH.Q ([TH.Name], HList.T (TypedActArgs us))
    unTypeArgs :: Proxy# us -> HList.T (TypedActArgs us) -> HList.T (UntypedActArgs us)

instance SemActArgs '[] where
    type TypedActArgs '[] = '[]
    type UntypedActArgs '[] = '[]

    unsafeSemActArgs _ = pure ([], HList.HNil)
    unTypeArgs _ HList.HNil = HList.HNil

instance SemActArgs us => SemActArgs (u ': us) where
    type TypedActArgs (u ': us) = TH.Q (TH.TExp u) ': TypedActArgs us
    type UntypedActArgs (u ': us) = TH.Q TH.Exp ': UntypedActArgs us

    unsafeSemActArgs _ = do
        n <- TH.newName "pteraTHSemActArg"
        let ne = TH.unsafeTExpCoerce do pure do TH.VarE n
        let arg = [|| pteraTHUnsafeCoerce $$(ne) ||]
        (ns, args) <- unsafeSemActArgs do proxy# :: Proxy# us
        pure (n:ns, arg HList.:* args)
    unTypeArgs _ = \case
        u HList.:* us -> fmap TH.unType u HList.:* unTypeArgs
            do proxy# :: Proxy# us
            us
