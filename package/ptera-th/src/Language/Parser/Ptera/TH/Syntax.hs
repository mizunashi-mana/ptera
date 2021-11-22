module Language.Parser.Ptera.TH.Syntax (
    T,

    Grammar (..),
    SafeGrammar.MemberInitials,
    Rules,
    RuleExpr,
    Alt,
    SafeGrammar.Expr,
    SafeGrammar.Unit,
    SemAct (..),

    SafeGrammar.fixGrammar,
    SafeGrammar.ruleExpr,
    SafeGrammar.alt,
    (SafeGrammar.<^>),
    (SafeGrammar.<:>),
    SafeGrammar.eps,
    SafeGrammar.var,
    SafeGrammar.varA,
) where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Data.HList         as HList
import qualified Language.Parser.Ptera.Data.Record        as Record
import qualified Language.Parser.Ptera.Data.TypeOps         as TypeOps
import qualified Language.Parser.Ptera.Syntax.SafeGrammar as SafeGrammar
import qualified Language.Haskell.TH as TH


type T = Grammar

data Grammar s h t e = Grammar
    {
        grammarMain :: SafeGrammar.Grammar SemAct s h e,
        grammarToken :: Record.T (TokensTag t)
    }

type family TokensTag (q :: [t]) :: [(t, Type)] where
    TokensTag '[] = '[]
    TokensTag (n ': ns) = '(n, TH.Q TH.Pat) ': TokensTag ns

type Rules h e = SafeGrammar.Rules SemAct h e
type RuleExpr = SafeGrammar.RuleExpr SemAct
type Alt = SafeGrammar.Alt SemAct

newtype SemAct us a = SemAct
    {
        semanticAction :: HList.T (TypeOps.Map TH.TExp us) -> TH.Q (TH.TExp a)
    }

tok ::
