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
import           Language.Parser.Ptera.Scanner                 (ListScanner (..),
                                                                Scanner (..),
                                                                runListScanner)
import           Language.Parser.Ptera.Syntax                  (Grammar, Alt, Expr,
                                                                Grammar,
                                                                GrammarToken (..),
                                                                Unit, RuleExpr,
                                                                eps, alt,
                                                                tok, var, ruleExpr,
                                                                fixGrammar,
                                                                (<:>), (<^>))

type Parser = Runner.T

genRunner :: GrammarToken e => Grammar s h e -> Maybe (Parser s h e)
genRunner g = Grammar2Runner.grammar2Runner g
