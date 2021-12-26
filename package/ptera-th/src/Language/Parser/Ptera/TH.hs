{-# LANGUAGE TemplateHaskell #-}

module Language.Parser.Ptera.TH (
    module Language.Parser.Ptera.TH.Syntax,
    module Language.Parser.Ptera.Scanner,
    module Language.Parser.Ptera.Runner,
    module Language.Parser.Ptera.TH.ParserLib,
    module Language.Parser.Ptera.TH.Pipeline.Grammar2ParserDec,
    genRunner,
    GenParam (..),
    defaultCustomCtxTy,
) where

import           Language.Parser.Ptera.Prelude

import qualified Language.Haskell.TH                                 as TH
import           Language.Parser.Ptera.Runner                        (Result (..),
                                                                      runParser)
import           Language.Parser.Ptera.Scanner                       hiding (T)
import           Language.Parser.Ptera.TH.ParserLib
import           Language.Parser.Ptera.TH.Pipeline.Grammar2ParserDec (TokenBounded)
import qualified Language.Parser.Ptera.TH.Pipeline.Grammar2ParserDec as Grammar2ParserDec
import           Language.Parser.Ptera.TH.Syntax                     hiding (T,
                                                                      UnsafeSemActM,
                                                                      unsafeSemanticAction)

genRunner :: forall vars rules tokens ctx elem.
    GrammarToken elem tokens => TokenBounded tokens
    => GenParam -> GrammarM ctx vars rules tokens elem -> TH.Q [TH.Dec]
genRunner param g = case mdecs of
        Nothing ->
            fail "Failed to generate parser"
        Just decs ->
            decs
    where
        mdecs = Grammar2ParserDec.grammar2ParserDec
            do Grammar2ParserDec.PipelineParam
                {
                    startsTy = startsTy param,
                    rulesTy = rulesTy param,
                    tokensTy = tokensTy param,
                    tokenTy = tokenTy param,
                    customCtxTy = customCtxTy param
                }
            do g

data GenParam = GenParam
    {
        startsTy :: TH.Q TH.Type,
        rulesTy  :: TH.Q TH.Type,
        tokensTy :: TH.Q TH.Type,
        tokenTy  :: TH.Q TH.Type,
        customCtxTy :: TH.Q TH.Type
    }

defaultCustomCtxTy :: TH.Q TH.Type
defaultCustomCtxTy = [t|()|]
