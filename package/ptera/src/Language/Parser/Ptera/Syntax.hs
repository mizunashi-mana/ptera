module Language.Parser.Ptera.Syntax (
    T,

    Grammar,
    Rule,
    Alt,
    Unit,
    Expr,
    SemanticAction (..),

    initial,
    rule,
    (SyntaxGrammar.<^>),
    (SyntaxGrammar.<^^>),
    (SyntaxGrammar.<:>),
    eps,
    var,
    tok,
) where

import Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Data.HList as HList
import qualified Language.Parser.Ptera.Syntax.Grammar as SyntaxGrammar
import qualified Language.Parser.Ptera.Syntax.SafeRule as SafeRule

type T s n t e = Grammar s n t e

type Grammar s n t e = SyntaxGrammar.T s n t e SemanticAction Identity
type Rule = SyntaxGrammar.Rule
type Alt n t e = SafeRule.Alt n t e SemanticAction
type Expr = SafeRule.Expr
type Unit = SafeRule.Unit

newtype SemanticAction us a = SemanticAction
    {
        semanticAction :: HList.T us -> a
    }

initial :: Enum s => s -> Grammar s n t e (Rule n r) -> Grammar s n t e ()
initial = SyntaxGrammar.initialT

rule :: Enum n => n -> [Grammar s n t e (Alt n t e r)] -> Grammar s n t e (Rule n r)
rule = SyntaxGrammar.ruleT

eps :: Grammar s n t e (Expr n t e '[])
eps = SyntaxGrammar.epsT

var :: Grammar s n t e (Rule n r) -> Grammar s n t e (Unit n t e r)
var = SyntaxGrammar.varT

tok :: t -> Grammar s n t e (Unit n t e e)
tok = SyntaxGrammar.tokenT
