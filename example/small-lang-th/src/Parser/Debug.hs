{-# LANGUAGE TemplateHaskell #-}

module Parser.Debug where

import qualified Language.Haskell.TH                as TH
import qualified Language.Parser.Ptera.TH           as PteraTH
import           Language.Parser.Ptera.TH.ParserLib
import qualified Parser.Rules                       as Rules
import           Types

showProgram :: IO String
showProgram = do
    p <- TH.runQ $ PteraTH.genRunner
        (PteraTH.GenParam {
            PteraTH.startsTy = [t|Rules.ParsePoints|],
            PteraTH.rulesTy  = [t|Rules.Rules|],
            PteraTH.tokensTy = [t|Rules.Tokens|],
            PteraTH.tokenTy  = [t|Token|]
        })
        Rules.grammar
    pure $ TH.pprint p
