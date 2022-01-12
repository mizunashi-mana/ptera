{-# LANGUAGE UndecidableInstances #-}

module Language.Parser.Ptera.TH.Pipeline.Grammar2ParserDec where

import           Language.Parser.Ptera.Prelude

import qualified Language.Haskell.TH                             as TH
import qualified Language.Parser.Ptera.Pipeline.SafeGrammar2SRB  as SafeGrammar2SRB
import qualified Language.Parser.Ptera.TH.Pipeline.SRB2ParserDec as SRB2ParserDec
import qualified Language.Parser.Ptera.TH.Syntax                 as Syntax
import qualified Prettyprinter
import qualified Type.Membership                                 as Membership

grammar2ParserDec
    :: forall initials rules tokens ctx elem
    .  Syntax.GrammarToken tokens elem
    => Membership.Generate (Syntax.TokensTag tokens)
    => PipelineParam
    -> Syntax.GrammarM ctx rules tokens elem initials
    -> TH.Q [TH.Dec]
grammar2ParserDec param g = do
    srb <- case SafeGrammar2SRB.safeGrammar2Srb g of
        Right x -> pure x
        Left vs -> do
            let errorMsg = Prettyprinter.hsep
                    [ Prettyprinter.pretty "Failed to generate parser."
                    , Prettyprinter.pretty "Detect left recursions at "
                        <> Prettyprinter.pretty vs
                        <> Prettyprinter.pretty "."
                    ]
            fail do show errorMsg
    SRB2ParserDec.srb2QParser
        do SRB2ParserDec.PipelineParam
            {
                startsTy = startsTy param,
                rulesTy = rulesTy param,
                tokensTy = tokensTy param,
                tokenTy = tokenTy param,
                customCtxTy = customCtxTy param,
                tokenBounds =
                    ( 0
                    , Membership.hcount do Proxy @(Syntax.TokensTag tokens)
                    )
            }
        do srb

data PipelineParam = PipelineParam
    {
        startsTy    :: TH.Q TH.Type,
        rulesTy     :: TH.Q TH.Type,
        tokensTy    :: TH.Q TH.Type,
        tokenTy     :: TH.Q TH.Type,
        customCtxTy :: TH.Q TH.Type
    }
