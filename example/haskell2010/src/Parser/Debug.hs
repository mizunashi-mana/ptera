{-# LANGUAGE TemplateHaskellQuotes #-}

module Parser.Debug where

import           Language.Parser.Ptera.Prelude

import qualified Language.Haskell.TH                             as TH
import qualified Language.Parser.Ptera.Machine.LAPEG             as LAPEG
import qualified Language.Parser.Ptera.Machine.PEG               as PEG
import qualified Language.Parser.Ptera.Machine.SRB               as SRB
import qualified Language.Parser.Ptera.Pipeline.Grammar2PEG      as Grammar2PEG
import qualified Language.Parser.Ptera.Pipeline.LAPEG2SRB        as LAPEG2SRB
import qualified Language.Parser.Ptera.Pipeline.PEG2LAPEG        as PEG2LAPEG
import qualified Language.Parser.Ptera.Syntax.SafeGrammar        as SafeGrammar
import qualified Language.Parser.Ptera.TH                        as PteraTH
import qualified Language.Parser.Ptera.TH.Pipeline.SRB2ParserDec as SRB2ParserDec
import qualified Parser.Rules                                    as Rules
import           Types

type SemanticAction = SRB2ParserDec.SemanticAction Rules.GrammarContext

peg :: PEG.T Int String (Maybe ()) SemanticAction
peg = Grammar2PEG.grammar2Peg $ SafeGrammar.unsafeGrammar Rules.grammar

laPeg :: LAPEG.T Int String (Maybe ()) SemanticAction
laPeg = case runExcept $ PEG2LAPEG.peg2LaPeg peg of
    Right x -> x
    Left vs -> error $ "unreachable: " ++ show vs

srb :: SRB.T Int String (Maybe ()) SemanticAction
srb = LAPEG2SRB.laPeg2Srb laPeg

showProgram :: IO String
showProgram = do
    p <- TH.runQ $ PteraTH.genRunner
        (PteraTH.GenParam
            {
                PteraTH.startsTy = [t|Rules.ParsePoints|],
                PteraTH.rulesTy  = [t|Rules.RuleDefs|],
                PteraTH.tokensTy = [t|Rules.Tokens|],
                PteraTH.tokenTy  = [t|Token|],
                PteraTH.customCtxTy = [t|Rules.GrammarContext|]
            })
        Rules.grammar
    pure $ TH.pprint p
