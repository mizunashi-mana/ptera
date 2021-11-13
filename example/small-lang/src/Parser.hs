{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

module Parser where

import           Data.Proxy                    (Proxy (..))
import qualified Language.Parser.Ptera         as Ptera
import qualified Language.Parser.Ptera.Scanner as Scanner
import qualified Parser.Rules                  as Rules
import           Types

exprParser :: Ptera.Scanner p Token m => m (Ptera.Result Ast)
exprParser = Ptera.runParser (Proxy :: Proxy "expr") runner where
    runner = case Ptera.genRunner Rules.grammar of
        Nothing -> error "unreachable"
        Just x  -> x

parseExpr :: [Token] -> Either String Ast
parseExpr toks = case Scanner.runListScanner exprParser toks of
    Ptera.Parsed x  -> Right x
    Ptera.ParseFail -> Left "ParseFail"
