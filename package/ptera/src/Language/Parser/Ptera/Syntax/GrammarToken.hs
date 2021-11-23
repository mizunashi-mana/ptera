module Language.Parser.Ptera.Syntax.GrammarToken where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Data.HEnum         as HEnum


type T = GrammarToken

class GrammarToken e q where
    tokenToTerminal :: Proxy q -> e -> HEnum.T q
