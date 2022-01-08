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
    (PteraTH.GenParam
        {
            PteraTH.startsTy = [t|Rules.ParsePoints|],
            PteraTH.rulesTy  = [t|Rules.Rules|],
            PteraTH.tokensTy = [t|Rules.Tokens|],
            PteraTH.tokenTy  = [t|Token|],
            PteraTH.customCtxTy = PteraTH.defaultCustomCtxTy
        })
    Rules.grammar
    )

exprParser :: PteraTH.Scanner posMark Token m => m (PteraTH.Result Ast)
exprParser = PteraTH.runParser (Proxy :: Proxy "expr") pteraTHRunner

parseExpr :: [Token] -> Either String Ast
parseExpr toks = case PteraTH.runListScanner exprParser toks of
    PteraTH.Parsed x  -> Right x
    PteraTH.ParseFail -> Left "ParseFail"
