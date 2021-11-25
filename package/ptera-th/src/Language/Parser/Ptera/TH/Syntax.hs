{-# LANGUAGE TemplateHaskell #-}

module Language.Parser.Ptera.TH.Syntax (
    T,

    Grammar,
    SafeGrammar.MemberInitials,
    Rules,
    RuleExpr,
    Alt,
    SafeGrammar.Expr,
    SafeGrammar.Unit,
    GrammarToken.GrammarToken (..),
    SemAct (..),
    SemActArgs (..),
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

import           Language.Parser.Ptera.Prelude

import qualified Language.Haskell.TH                       as TH
import qualified Language.Haskell.TH.Syntax                as TH
import qualified Language.Parser.Ptera.Data.HList          as HList
import qualified Language.Parser.Ptera.Syntax.GrammarToken as GrammarToken
import qualified Language.Parser.Ptera.Syntax.SafeGrammar  as SafeGrammar
import           Language.Parser.Ptera.TH.ParserLib


type T = Grammar

type Grammar = SafeGrammar.Grammar SemAct

type family TokensTag (q :: [t]) :: [(t, Type)] where
    TokensTag '[] = '[]
    TokensTag (n ': ns) = '(n, TH.Q TH.Pat) ': TokensTag ns

type Rules h q e = SafeGrammar.Rules SemAct h q e
type RuleExpr = SafeGrammar.RuleExpr SemAct
type Alt = SafeGrammar.Alt SemAct

newtype SemAct us a = UnsafeSemAct
    {
        unsafeSemanticAction :: TH.Q TH.Exp
    }

semAct :: forall us a. SemActArgs us
    => (HList.T (ActArgs us) -> TH.Q (TH.TExp a)) -> SemAct us a
semAct f = UnsafeSemAct do
    (ns, args) <- unsafeSemActArgs do proxy# :: Proxy# us
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

class SemActArgs (us :: [Type]) where
    type ActArgs us :: [Type]

    unsafeSemActArgs :: Proxy# us -> TH.Q ([TH.Name], HList.T (ActArgs us))

instance SemActArgs '[] where
    type ActArgs '[] = '[]

    unsafeSemActArgs _ = pure ([], HList.HNil)

instance SemActArgs us => SemActArgs (u ': us) where
    type ActArgs (u ': us) = TH.Q (TH.TExp u) ': ActArgs us

    unsafeSemActArgs _ = do
        n <- TH.newName "pteraTHSemActArg"
        let ne = TH.unsafeTExpCoerce do pure do TH.VarE n
        let arg = [|| pteraTHUnsafeCoerce $$(ne) ||]
        (ns, args) <- unsafeSemActArgs do proxy# :: Proxy# us
        pure (n:ns, arg HList.:* args)
