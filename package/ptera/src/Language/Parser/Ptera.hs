module Language.Parser.Ptera (
    module Language.Parser.Ptera.Syntax,
    module Language.Parser.Ptera.Runner,
    module Language.Parser.Ptera.Scanner,

    Parser,
    genRunner,
) where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Pipeline.Grammar2Runner as Grammar2Runner
import           Language.Parser.Ptera.Runner                  (ParseResult (..),
                                                                Result,
                                                                runParser)
import qualified Language.Parser.Ptera.Runner                  as Runner
import           Language.Parser.Ptera.Scanner                 hiding (T)
import           Language.Parser.Ptera.Syntax                  hiding (T, semAct, semActM)

type Parser = Runner.T

genRunner :: GrammarToken tokens elem
    => GrammarM ctx rules tokens elem initials
    -> Either [StringLit] (Parser ctx rules elem initials)
genRunner g = Grammar2Runner.grammar2Runner g
