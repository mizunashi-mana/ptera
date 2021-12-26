{-# LANGUAGE UndecidableInstances #-}

module Language.Parser.Ptera.TH.Pipeline.Grammar2ParserDec where

import           Language.Parser.Ptera.Prelude

import qualified GHC.TypeNats                                    as TypeNats
import qualified Language.Haskell.TH                             as TH
import qualified Language.Parser.Ptera.Data.TypeOps              as TypeOps
import qualified Language.Parser.Ptera.Pipeline.SafeGrammar2SRB  as SafeGrammar2SRB
import qualified Language.Parser.Ptera.TH.Pipeline.SRB2ParserDec as SRB2ParserDec
import qualified Language.Parser.Ptera.TH.Syntax                 as Syntax

grammar2ParserDec :: forall vars rules tokens ctx elem.
    Syntax.GrammarToken elem tokens => TokenBounded tokens
    => PipelineParam -> Syntax.GrammarM ctx vars rules tokens elem -> Maybe (TH.Q [TH.Dec])
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
                    tokenBounds = tokenBounds do Proxy @tokens
                }
            do srb

class TokenBounded tokens where
    tokenBounds :: Proxy tokens -> (Int, Int)

instance TypeNats.KnownNat (TypeOps.Length tokens) => TokenBounded tokens where
    tokenBounds Proxy = do
        let tokMax = fromInteger do
                toInteger do
                    TypeNats.natVal' do proxy# :: Proxy# (TypeOps.Length tokens)
        (0, tokMax)

data PipelineParam = PipelineParam
    {
        startsTy :: TH.Q TH.Type,
        rulesTy  :: TH.Q TH.Type,
        tokensTy :: TH.Q TH.Type,
        tokenTy  :: TH.Q TH.Type,
        customCtxTy :: TH.Q TH.Type
    }
