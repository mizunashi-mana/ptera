module Language.Parser.Ptera.Pipeline.Grammar2Runner where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Pipeline.Grammar2PEG as Grammar2PEG
import qualified Language.Parser.Ptera.Pipeline.LAPEG2SRB   as LAPEG2SRB
import qualified Language.Parser.Ptera.Pipeline.PEG2LAPEG   as PEG2LAPEG
import qualified Language.Parser.Ptera.Pipeline.SRB2Parser  as SRB2Parser
import qualified Language.Parser.Ptera.Runner               as Runner
import qualified Language.Parser.Ptera.Syntax               as Syntax

grammar2Runner :: forall h n e. Enum n => Syntax.GrammarToken e
    => Syntax.FixedGrammar h n e -> Maybe (Runner.T h e)
grammar2Runner (Syntax.UnsafeFixedGrammar g) = do
    let peg = Grammar2PEG.grammar2Peg g
    laPeg <- case runExcept do PEG2LAPEG.peg2LaPeg peg of
        Left{}  -> Nothing
        Right x -> Just x
    let srb = LAPEG2SRB.laPeg2Srb laPeg
    let genParam = SRB2Parser.GenParam
            {
                genParamGetToken = \tok -> fromEnum do Syntax.tokenToTerminal @e tok
            }
    let parser = SRB2Parser.srb2Parser genParam srb
    pure do Runner.UnsafeRunner parser
