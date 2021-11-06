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
    alt,
    (SyntaxGrammar.<^>),
    (<:>),
    eps,
    var,
    tok,
) where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Data.HList      as HList
import qualified Language.Parser.Ptera.Data.Member     as Member
import qualified Language.Parser.Ptera.Data.Record     as Record
import qualified Language.Parser.Ptera.Data.TypeOps    as TypeOps
import qualified Language.Parser.Ptera.Syntax.Grammar  as SyntaxGrammar
import qualified Language.Parser.Ptera.Syntax.SafeRule as SafeRule

type T = Grammar

newtype Grammar :: [(k, Type)] -> Type -> Type -> Type -> Type -> Type where
    Grammar:: SyntaxGrammar.T Int n t e SemanticAction Identity a -> Grammar h n t e a

type Rule = SyntaxGrammar.Rule
type Alt n t e = SafeRule.Alt n t e SemanticAction
type Expr = SafeRule.Expr
type Unit = SafeRule.Unit

newtype SemanticAction us a = SemanticAction
    {
        semanticAction :: HList.T us -> a
    }

initial :: forall k h n t e r. r ~ TypeOps.FromJust (Record.RecordIndex k h)
    => Member.T k (TypeOps.MapFst h)
    => Proxy# k -> Grammar h n t e (Rule n r) -> Grammar h n t e ()
initial kp (Grammar g) = Grammar do
    SyntaxGrammar.initialT
        do Member.position kp do proxy# :: Proxy# (TypeOps.MapFst h)
        do g

rule :: Enum n => n -> [Grammar h n t e (Alt n t e r)] -> Grammar h n t e (Rule n r)
rule v alts = Grammar do SyntaxGrammar.ruleT v do coerce alts

alt :: Grammar s n t e (Expr n t e us, SemanticAction us r) -> Grammar s n t e (Alt n t e r)
alt (Grammar eact) = Grammar do SyntaxGrammar.altT eact

(<:>) :: Grammar s n t e (Unit n t e u) -> (HList.T us2 -> r)
    -> Grammar s n t e (Expr n t e '[u], SemanticAction us2 r)
Grammar mu <:> act = Grammar do mu SyntaxGrammar.<:> SemanticAction act

infixr 5 <:>

eps :: SemanticAction '[] r -> Grammar s n t e (Alt n t e r)
eps act = Grammar do SyntaxGrammar.epsT act

var :: Grammar s n t e (Rule n r) -> Grammar s n t e (Unit n t e r)
var (Grammar r) = Grammar do SyntaxGrammar.varT r

tok :: t -> Grammar s n t e (Unit n t e e)
tok t = Grammar do SyntaxGrammar.tokenT t
