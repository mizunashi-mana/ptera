module Language.Parser.Ptera.Syntax where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Data.HList      as HList
import qualified Language.Parser.Ptera.Data.Member     as Member
import qualified Language.Parser.Ptera.Data.Record     as Record
import qualified Language.Parser.Ptera.Data.TypeOps    as TypeOps
import qualified Language.Parser.Ptera.Syntax.Grammar  as SyntaxGrammar
import qualified Language.Parser.Ptera.Syntax.SafeRule as SafeRule

type T = GrammarT

newtype GrammarT (h :: [(k, Type)]) n t e m a =
    UnsafeGrammarT (SyntaxGrammar.T Int n t e SemanticAction m a)
    deriving (Functor, Applicative, Monad) via SyntaxGrammar.T Int n t e SemanticAction m


fixedT :: Monad m => Enum t => (e -> t) -> GrammarT h n t e m () -> m (FixedGrammar h n t e)
fixedT getToken (UnsafeGrammarT g) = do
    fixedG <- SyntaxGrammar.fixedT g
    pure do
        UnsafeFixedGrammar
            {
                unsafeGrammarGetToken = getToken,
                unsafeGrammarFixed = fixedG
            }

data FixedGrammar (h :: [(k, Type)]) n t e = UnsafeFixedGrammar
    {
        unsafeGrammarGetToken :: e -> t,
        unsafeGrammarFixed :: SyntaxGrammar.FixedGrammar Int n t e SemanticAction
    }

type Rule = SyntaxGrammar.Rule
type Alt n t e = SafeRule.Alt n t e SemanticAction
type Expr = SafeRule.Expr
type Unit = SafeRule.Unit

newtype SemanticAction us a = SemanticAction
    {
        semanticAction :: HList.T us -> a
    }

initial :: forall k h n t e m r. Monad m
    => r ~ TypeOps.FromJust (Record.RecordIndex k h)
    => Member.T k (TypeOps.MapFst h)
    => Proxy# k -> GrammarT h n t e m (Rule n r) -> GrammarT h n t e m ()
initial kp (UnsafeGrammarT g) = UnsafeGrammarT do
    SyntaxGrammar.initialT
        do Member.position kp do proxy# :: Proxy# (TypeOps.MapFst h)
        do g

rule :: Monad m => Enum n
    => n -> [GrammarT h n t e m (Alt n t e r)] -> GrammarT h n t e m (Rule n r)
rule v alts = UnsafeGrammarT do SyntaxGrammar.ruleT v do coerce alts

alt :: Monad m
    => GrammarT h n t e m (Expr n t e us, SemanticAction us r) -> GrammarT h n t e m (Alt n t e r)
alt (UnsafeGrammarT eact) = UnsafeGrammarT do SyntaxGrammar.altT eact

(<^>) :: Monad m
    => GrammarT h n t e m (Unit n t e u)
    -> GrammarT h n t e m (Expr n t e us1, SemanticAction us2 r)
    -> GrammarT h n t e m (Expr n t e (u ': us1), SemanticAction us2 r)
UnsafeGrammarT mu <^> UnsafeGrammarT mus = UnsafeGrammarT do mu SyntaxGrammar.<^> mus

infixr 5 <^>

(<:>) :: Monad m
    => GrammarT h n t e m (Unit n t e u) -> (HList.T us2 -> r)
    -> GrammarT h n t e m (Expr n t e '[u], SemanticAction us2 r)
UnsafeGrammarT mu <:> act = UnsafeGrammarT do mu SyntaxGrammar.<:> SemanticAction act

infixr 5 <:>

eps :: Monad m => SemanticAction '[] r -> GrammarT h n t e m (Alt n t e r)
eps act = UnsafeGrammarT do SyntaxGrammar.epsT act

var :: Monad m
    => GrammarT h n t e m (Rule n r) -> GrammarT h n t e m (Unit n t e r)
var (UnsafeGrammarT r) = UnsafeGrammarT do SyntaxGrammar.varT r

tok :: Monad m => t -> GrammarT h n t e m (Unit n t e e)
tok t = UnsafeGrammarT do SyntaxGrammar.tokenT t
