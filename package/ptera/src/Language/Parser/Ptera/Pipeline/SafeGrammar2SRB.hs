module Language.Parser.Ptera.Pipeline.SafeGrammar2SRB where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Machine.SRB          as SRB
import qualified Language.Parser.Ptera.Pipeline.Grammar2PEG as Grammar2PEG
import qualified Language.Parser.Ptera.Pipeline.LAPEG2SRB   as LAPEG2SRB
import qualified Language.Parser.Ptera.Pipeline.PEG2LAPEG   as PEG2LAPEG
import qualified Language.Parser.Ptera.Syntax.Grammar       as Grammar
import qualified Language.Parser.Ptera.Syntax.SafeGrammar   as SafeGrammar

safeGrammar2Srb :: SafeGrammar.Grammar action vars rules tokens elem
    -> Maybe (SRB.T Int (Grammar.Action action))
safeGrammar2Srb (SafeGrammar.UnsafeGrammar g) = do
    let peg = Grammar2PEG.grammar2Peg g
    laPeg <- case runExcept do PEG2LAPEG.peg2LaPeg peg of
        Left{}  -> Nothing
        Right x -> Just x
    pure do LAPEG2SRB.laPeg2Srb laPeg
