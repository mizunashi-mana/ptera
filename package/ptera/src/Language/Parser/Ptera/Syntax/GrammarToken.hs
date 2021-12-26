module Language.Parser.Ptera.Syntax.GrammarToken where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Data.HEnum as HEnum


type T = GrammarToken

class GrammarToken elem tokens where
    tokenToTerminal :: Proxy tokens -> elem -> HEnum.T tokens
