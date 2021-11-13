module Language.Parser.Ptera.Syntax where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Data.HList      as HList
import qualified Language.Parser.Ptera.Data.Member     as Member
import qualified Language.Parser.Ptera.Data.Record     as Record
import qualified Language.Parser.Ptera.Data.TypeOps    as TypeOps
import qualified Language.Parser.Ptera.Syntax.Grammar  as SyntaxGrammar
import qualified Language.Parser.Ptera.Syntax.SafeRule as SafeRule

type T = GrammarT

newtype GrammarT (h :: [(k, Type)]) n e m a =
    UnsafeGrammarT (SyntaxGrammar.T Int n (Terminal e) e SemanticAction m a)
    deriving (Functor, Applicative, Monad)
        via SyntaxGrammar.T Int n (Terminal e) e SemanticAction m


fixedT :: Monad m => GrammarT h n e m () -> m (FixedGrammar h n e)
fixedT (UnsafeGrammarT g) = do
    fixedG <- SyntaxGrammar.fixedT g
    pure do UnsafeFixedGrammar fixedG

newtype FixedGrammar (h :: [(k, Type)]) n e = UnsafeFixedGrammar
    {
        unsafeFixedGrammar :: SyntaxGrammar.FixedGrammar Int n (Terminal e) e SemanticAction
    }

class Enum (Terminal e) => GrammarToken e where
    data Terminal e :: Type

    tokenToTerminal :: e -> Terminal e

type Rule = SyntaxGrammar.Rule
type Alt n e = SafeRule.Alt n (Terminal e) e SemanticAction
type Expr n e = SafeRule.Expr n (Terminal e) e
type Unit n e = SafeRule.Unit n (Terminal e) e

newtype SemanticAction us a = SemanticAction
    {
        semanticAction :: HList.T us -> a
    }

initial :: forall k h n e m r. Monad m
    => r ~ TypeOps.FromJust (Record.RecordIndex k h)
    => Member.T k (TypeOps.MapFst h)
    => Proxy k -> GrammarT h n e m (Rule n r) -> GrammarT h n e m ()
initial _ (UnsafeGrammarT g) = UnsafeGrammarT do
    SyntaxGrammar.initialT
        do Member.position
            do proxy# :: Proxy# k
            do proxy# :: Proxy# (TypeOps.MapFst h)
        do g

rule :: Monad m => Enum n
    => n -> [GrammarT h n e m (Alt n e r)] -> GrammarT h n e m (Rule n r)
rule v alts = UnsafeGrammarT do SyntaxGrammar.ruleT v do coerce alts

alt :: Monad m
    => GrammarT h n e m (Expr n e us, SemanticAction us r) -> GrammarT h n e m (Alt n e r)
alt (UnsafeGrammarT eact) = UnsafeGrammarT do SyntaxGrammar.altT eact

(<^>) :: Monad m
    => GrammarT h n e m (Unit n e u)
    -> GrammarT h n e m (Expr n e us1, SemanticAction us2 r)
    -> GrammarT h n e m (Expr n e (u ': us1), SemanticAction us2 r)
UnsafeGrammarT mu <^> UnsafeGrammarT mus = UnsafeGrammarT do mu SyntaxGrammar.<^> mus

infixr 5 <^>

(<:>) :: Monad m
    => GrammarT h n e m (Unit n e u) -> (HList.T us2 -> r)
    -> GrammarT h n e m (Expr n e '[u], SemanticAction us2 r)
UnsafeGrammarT mu <:> act = UnsafeGrammarT do mu SyntaxGrammar.<:> SemanticAction act

infixr 5 <:>

eps :: Monad m => SemanticAction '[] r -> GrammarT h n e m (Alt n e r)
eps act = UnsafeGrammarT do SyntaxGrammar.epsT act

var :: Monad m
    => GrammarT h n e m (Rule n r) -> GrammarT h n e m (Unit n e r)
var (UnsafeGrammarT r) = UnsafeGrammarT do SyntaxGrammar.varT r

tok :: Monad m => Terminal e -> GrammarT h n e m (Unit n e e)
tok t = UnsafeGrammarT do SyntaxGrammar.tokenT t
