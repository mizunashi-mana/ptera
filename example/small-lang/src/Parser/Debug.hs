module Parser.Debug where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Machine.LAPEG        as LAPEG
import qualified Language.Parser.Ptera.Machine.PEG          as PEG
import qualified Language.Parser.Ptera.Machine.SRB          as SRB
import qualified Language.Parser.Ptera.Pipeline.Grammar2PEG as Grammar2PEG
import qualified Language.Parser.Ptera.Pipeline.LAPEG2SRB   as LAPEG2SRB
import qualified Language.Parser.Ptera.Pipeline.PEG2LAPEG   as PEG2LAPEG
import qualified Language.Parser.Ptera.Runner.Parser        as RunnerParser
import qualified Language.Parser.Ptera.Syntax               as Syntax
import qualified Parser.Rules                               as Rules
import qualified Types

peg :: PEG.T Int RunnerParser.Action
peg = Grammar2PEG.grammar2Peg $ Syntax.unsafeGrammar Rules.grammar

laPeg :: LAPEG.T Int RunnerParser.Action
laPeg = case runExcept $ PEG2LAPEG.peg2LaPeg peg of
    Right x -> x
    Left vs -> error $ "unreachable: " ++ show vs

srb :: SRB.T Int RunnerParser.Action
srb = LAPEG2SRB.laPeg2Srb laPeg
