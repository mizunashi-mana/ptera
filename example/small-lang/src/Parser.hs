{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

module Parser where

import           Data.Proxy            (Proxy (..))
import qualified Language.Parser.Ptera as Ptera
import qualified Parser.Rules          as Rules

parseExpr :: Ptera.Scanner p Rules.Token m => m (Ptera.Result Rules.Ast)
parseExpr = Ptera.runParser (Proxy :: Proxy "expr") runner where
    runner = case Ptera.genRunner Rules.grammar of
        Nothing -> error "unreachable"
        Just x  -> x
