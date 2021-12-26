module Language.Parser.Ptera (
    module Language.Parser.Ptera.Syntax,
    module Language.Parser.Ptera.Runner,
    module Language.Parser.Ptera.Scanner,

    Parser,
    genRunner,
) where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Pipeline.Grammar2Runner as Grammar2Runner
import           Language.Parser.Ptera.Runner                  (Result (..),
                                                                runParser)
import qualified Language.Parser.Ptera.Runner                  as Runner
import           Language.Parser.Ptera.Scanner                 hiding (T)
import           Language.Parser.Ptera.Syntax                  hiding (T)

type Parser = Runner.T

genRunner :: GrammarToken elem tokens
    => GrammarM ctx vars rules tokens elem -> Maybe (Parser ctx vars rules elem)
genRunner g = Grammar2Runner.grammar2Runner g
