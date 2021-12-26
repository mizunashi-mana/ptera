{-# LANGUAGE UndecidableInstances #-}

module Language.Parser.Ptera.TH.Pipeline.Grammar2ParserDec where

import           Language.Parser.Ptera.Prelude

import qualified GHC.TypeNats                                    as TypeNats
import qualified Language.Haskell.TH                             as TH
import qualified Language.Parser.Ptera.Data.TypeOps              as TypeOps
import qualified Language.Parser.Ptera.Pipeline.SafeGrammar2SRB  as SafeGrammar2SRB
import qualified Language.Parser.Ptera.TH.Pipeline.SRB2ParserDec as SRB2ParserDec
import qualified Language.Parser.Ptera.TH.Syntax                 as Syntax

grammarT2ParserDec :: forall m s h q e.
    Syntax.GrammarToken e q => TokenBounded q
    => PipelineParam -> Syntax.GrammarT m s h q e -> Maybe (TH.Q [TH.Dec])
grammarT2ParserDec param g = do
    srb <- SafeGrammar2SRB.safeGrammar2Srb g
    pure
        do SRB2ParserDec.srb2QParser
            do SRB2ParserDec.PipelineParam
                {
                    startsTy = startsTy param,
                    rulesTy = rulesTy param,
                    tokensTy = tokensTy param,
                    tokenTy = tokenTy param,
                    tokenBounds = tokenBounds do Proxy @q
                }
            do srb

class TokenBounded q where
    tokenBounds :: Proxy q -> (Int, Int)

instance TypeNats.KnownNat (TypeOps.Length q) => TokenBounded q where
    tokenBounds Proxy = do
        let tokMax = fromInteger do
                toInteger do
                    TypeNats.natVal' do proxy# :: Proxy# (TypeOps.Length q)
        (0, tokMax)

data PipelineParam = PipelineParam
    {
        startsTy :: TH.Q TH.Type,
        rulesTy  :: TH.Q TH.Type,
        tokensTy :: TH.Q TH.Type,
        tokenTy  :: TH.Q TH.Type
    }
