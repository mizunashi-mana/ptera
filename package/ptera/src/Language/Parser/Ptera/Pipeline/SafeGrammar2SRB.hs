module Language.Parser.Ptera.Pipeline.SafeGrammar2SRB where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Machine.SRB          as SRB
import qualified Language.Parser.Ptera.Pipeline.Grammar2PEG as Grammar2PEG
import qualified Language.Parser.Ptera.Pipeline.LAPEG2SRB   as LAPEG2SRB
import qualified Language.Parser.Ptera.Pipeline.PEG2LAPEG   as PEG2LAPEG
import qualified Language.Parser.Ptera.Syntax.Grammar       as Grammar
import qualified Language.Parser.Ptera.Syntax.SafeGrammar   as SafeGrammar
import qualified Language.Parser.Ptera.Machine.PEG as PEG
import qualified Language.Parser.Ptera.Data.Alignable.Set as AlignableSet
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray

safeGrammar2Srb :: SafeGrammar.Grammar action vars rules tokens elem
    -> Either [StringLit] (SRB.T Int StringLit (Grammar.Action action))
safeGrammar2Srb (SafeGrammar.UnsafeGrammar g) = do
    let peg = Grammar2PEG.grammar2Peg g
    laPeg <- case runExcept do PEG2LAPEG.peg2LaPeg peg of
        Right x -> Right x
        Left vs -> do
            let displayVs = PEG.displayVars peg
            Left
                [ AlignableArray.forceIndex displayVs v
                | v <- AlignableSet.toList vs
                ]
    pure do LAPEG2SRB.laPeg2Srb laPeg
