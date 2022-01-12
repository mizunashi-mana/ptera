module Language.Parser.Ptera.Pipeline.Grammar2Runner where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Pipeline.SRB2Parser      as SRB2Parser
import qualified Language.Parser.Ptera.Pipeline.SafeGrammar2SRB as SafeGrammar2SRB
import qualified Language.Parser.Ptera.Runner                   as Runner
import qualified Language.Parser.Ptera.Syntax                   as Syntax
import qualified Prettyprinter

grammar2Runner :: forall initials ctx rules tokens ann elem
    .  Syntax.GrammarToken tokens elem
    => Syntax.GrammarM ctx rules tokens elem initials
    -> Either (Prettyprinter.Doc ann) (Runner.T ctx rules elem initials)
grammar2Runner g = do
    srb <- case SafeGrammar2SRB.safeGrammar2Srb g of
        Right x -> Right x
        Left vs -> Left do
            Prettyprinter.pretty "Detect left recursions at" <> Prettyprinter.pretty vs
    let parser = SRB2Parser.srb2Parser
            do Proxy @tokens
            do srb
    pure do Runner.UnsafeRunnerM parser
