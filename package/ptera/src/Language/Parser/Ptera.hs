module Language.Parser.Ptera (
    module Language.Parser.Ptera.Syntax,
    module Language.Parser.Ptera.Runner,
    module Language.Parser.Ptera.Scanner,

    Parser,
    genRunnerT,
    Grammar,
    genRunner,
) where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Pipeline.Grammar2Runner as Grammar2Runner
import           Language.Parser.Ptera.Runner                  (Result (..),
                                                                runParser)
import qualified Language.Parser.Ptera.Runner                  as Runner
import           Language.Parser.Ptera.Scanner                 (Scanner (..))
import           Language.Parser.Ptera.Syntax                  (Alt, Expr,
                                                                GrammarT, Rule,
                                                                Unit, alt, eps,
                                                                initial, rule,
                                                                tok, var, (<:>),
                                                                (<^>))
import qualified Language.Parser.Ptera.Syntax                  as Syntax

type Parser = Runner.T

genRunnerT :: Monad m => Enum n => Enum t
    => (e -> t) -> GrammarT h n t e m () -> m (Maybe (Parser h e))
genRunnerT tokMap g = do
    fixedG <- Syntax.fixedT tokMap g
    pure do Grammar2Runner.grammar2Runner fixedG

type Grammar h n t e = GrammarT h n t e Identity

genRunner :: Enum n => Enum t
    => (e -> t) -> Grammar h n t e () -> Maybe (Parser h e)
genRunner tokMap g = runIdentity do genRunnerT tokMap g
