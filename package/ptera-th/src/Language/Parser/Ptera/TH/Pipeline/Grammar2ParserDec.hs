{-# LANGUAGE UndecidableInstances #-}

module Language.Parser.Ptera.TH.Pipeline.Grammar2ParserDec where

import           Language.Parser.Ptera.Prelude

import qualified Language.Haskell.TH                             as TH
import qualified Language.Parser.Ptera.Pipeline.SafeGrammar2SRB  as SafeGrammar2SRB
import qualified Language.Parser.Ptera.TH.Pipeline.SRB2ParserDec as SRB2ParserDec
import qualified Language.Parser.Ptera.TH.Syntax                 as Syntax
import qualified Type.Membership                                 as Membership

grammar2ParserDec
    :: forall initials rules tokens ctx elem
    .  Syntax.GrammarToken elem tokens => Membership.Generate tokens
    => PipelineParam -> Syntax.GrammarM ctx rules tokens elem initials -> Maybe (TH.Q [TH.Dec])
grammar2ParserDec param g = do
    srb <- SafeGrammar2SRB.safeGrammar2Srb g
    pure
        do SRB2ParserDec.srb2QParser
            do SRB2ParserDec.PipelineParam
                {
                    startsTy = startsTy param,
                    rulesTy = rulesTy param,
                    tokensTy = tokensTy param,
                    tokenTy = tokenTy param,
                    customCtxTy = customCtxTy param,
                    tokenBounds = (0, Membership.hcount do Proxy @tokens)
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
