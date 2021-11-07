{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Parser where

import qualified Parser.Rules as Rules
import qualified Language.Parser.Ptera as Ptera
import           Data.Proxy                       (Proxy (..))

parseExpr :: Ptera.Scanner p Rules.Token m => m (Ptera.Result Rules.Ast)
parseExpr = Ptera.runParser (Proxy :: Proxy "expr") runner where
    runner = case Ptera.genRunner Rules.grammar of
        Nothing -> error "unreachable"
        Just x  -> x
