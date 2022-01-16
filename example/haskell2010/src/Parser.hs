{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Parser where

import           Data.Proxy                         (Proxy (..))
import qualified Language.Parser.Ptera.TH           as PteraTH
import           Language.Parser.Ptera.TH.ParserLib
import qualified Parser.Rules                       as Rules
import           Types

$(PteraTH.genRunner
    (PteraTH.GenParam {
        PteraTH.startsTy = [t|Rules.ParsePoints|],
        PteraTH.rulesTy  = [t|Rules.Rules|],
        PteraTH.tokensTy = [t|Rules.Tokens|],
        PteraTH.tokenTy  = [t|Token|],
        PteraTH.customCtxTy = [t|Rules.GrammarContext|]
    })
    Rules.grammar
    )

moduleParser :: PteraTH.Scanner p Token m => m (PteraTH.Result p Program)
moduleParser = PteraTH.runParserM (Proxy :: Proxy "module EOS") pteraTHRunner []

parseModule :: [Token] -> Either String Program
parseModule toks = case PteraTH.runListScanner moduleParser toks of
    PteraTH.Parsed x ->
        Right x
    PteraTH.ParseFailed p e ->
        Left $ show ("ParseFail", e, p)
