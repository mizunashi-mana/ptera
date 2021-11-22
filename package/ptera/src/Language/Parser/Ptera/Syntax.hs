module Language.Parser.Ptera.Syntax (
    T,

    Grammar,
    SafeGrammar.MemberInitials,
    Rules,
    RuleExpr,
    Alt,
    SafeGrammar.Expr,
    SafeGrammar.Unit,
    GrammarToken (..),
    SemAct (..),

    SafeGrammar.fixGrammar,
    SafeGrammar.ruleExpr,
    SafeGrammar.alt,
    (SafeGrammar.<^>),
    (SafeGrammar.<:>),
    SafeGrammar.eps,
    SafeGrammar.var,
    SafeGrammar.varA,
    tok,
) where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Syntax.SafeGrammar as SafeGrammar
import qualified Language.Parser.Ptera.Data.HList     as HList


type T = Grammar

type Grammar = SafeGrammar.Grammar SemAct
type Rules h e = SafeGrammar.Rules SemAct h e
type RuleExpr = SafeGrammar.RuleExpr SemAct
type Alt = SafeGrammar.Alt SemAct


class Enum (Terminal e) => GrammarToken e where
    data Terminal e :: Type

    tokenToTerminal :: e -> Terminal e


newtype SemAct us a = SemAct
    {
        semanticAction :: HList.T us -> a
    }

tok :: GrammarToken e => Terminal e -> SafeGrammar.Unit h e e
tok t = SafeGrammar.tok do fromEnum t
