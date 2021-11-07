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
import           Language.Parser.Ptera.Scanner                 (Scanner (..), ListScanner (..), runListScanner)
import           Language.Parser.Ptera.Syntax                  (Alt, Expr,
                                                                GrammarT, Rule,
                                                                Unit, alt, eps,
                                                                initial, rule,
                                                                tok, var, (<:>),
                                                                (<^>),
                                                                GrammarToken (..))
import qualified Language.Parser.Ptera.Syntax                  as Syntax

type Parser = Runner.T

genRunnerT :: Monad m => Enum n => GrammarToken e t
    => GrammarT h n t e m () -> m (Maybe (Parser h e))
genRunnerT g = do
    fixedG <- Syntax.fixedT g
    pure do Grammar2Runner.grammar2Runner fixedG

type Grammar h n t e = GrammarT h n t e Identity

genRunner :: Enum n => GrammarToken e t
    => Grammar h n t e () -> Maybe (Parser h e)
genRunner g = runIdentity do genRunnerT g
