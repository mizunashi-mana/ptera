module Language.Parser.Ptera.Pipeline.Grammar2Runner where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Pipeline.SRB2Parser      as SRB2Parser
import qualified Language.Parser.Ptera.Pipeline.SafeGrammar2SRB as SafeGrammar2SRB
import qualified Language.Parser.Ptera.Runner                   as Runner
import qualified Language.Parser.Ptera.Syntax                   as Syntax

grammar2Runner :: forall vars rules tokens ctx elem.
    Syntax.GrammarToken elem tokens
    => Syntax.GrammarM ctx vars rules tokens elem -> Maybe (Runner.T ctx vars rules elem)
grammar2Runner g = do
    srb <- SafeGrammar2SRB.safeGrammar2Srb g
    let parser = SRB2Parser.srb2Parser
            do Proxy @tokens
            do srb
    pure do Runner.UnsafeRunnerM parser
