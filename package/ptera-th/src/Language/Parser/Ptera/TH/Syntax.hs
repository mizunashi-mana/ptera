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
import qualified Language.Parser.Ptera.Data.HList          as HList
import qualified Language.Parser.Ptera.Syntax.GrammarToken as GrammarToken
import qualified Language.Parser.Ptera.Syntax.SafeGrammar  as SafeGrammar


type T = Grammar

type Grammar = SafeGrammar.Grammar SemAct

type family TokensTag (q :: [t]) :: [(t, Type)] where
    TokensTag '[] = '[]
    TokensTag (n ': ns) = '(n, TH.Q TH.Pat) ': TokensTag ns

type Rules h q e = SafeGrammar.Rules SemAct h q e
type RuleExpr = SafeGrammar.RuleExpr SemAct
type Alt = SafeGrammar.Alt SemAct

newtype SemAct us a = SemAct
    {
        semanticAction :: HList.T (ActArgs us) -> TH.Q (TH.TExp a)
    }

type family ActArgs (us :: [Type]) :: [Type] where
    ActArgs '[] = '[]
    ActArgs (u ': us) = TH.Q (TH.TExp u) ': ActArgs us
