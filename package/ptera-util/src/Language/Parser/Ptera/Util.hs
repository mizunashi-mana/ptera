{-# LANGUAGE TemplateHaskell #-}

module Language.Parser.Ptera.Util (
    genGrammarToken,
) where

import Language.Parser.Ptera.Prelude
import Prelude (String)
import Language.Parser.Ptera.Syntax (GrammarToken (..))

import qualified Language.Haskell.TH as TH

genGrammarToken :: TH.Name -> TH.Q TH.Type -> [(String, TH.Q TH.Pat)] -> TH.Q [TH.Dec]
genGrammarToken tyName tokenTy tokens = sequence [tokensTyD, grammarTokenInstanceD]
    where
        tokensTyD = TH.TySynD tyName [] <$> foldr
            do \(tokenName, _) ty -> do
                let tokenLitTy = TH.LitT do TH.StrTyLit tokenName
                [t|$(pure tokenLitTy) ': $(ty)|]
            do [t|'[]|]
            do tokens

        grammarTokenInstanceD = TH.InstanceD Nothing []
            <$> [t|GrammarToken $(tokenTy) $(pure do TH.ConT tyName)|]
            <*> sequence
                [ TH.FunD
                    do TH.mkName "tokenToTerminal"
                    <$> sequence [tokenToTerminalClause]
                ]

        tokenToTerminalClause = do
            paramTokenName <- TH.newName "token"
            TH.Clause
                <$> sequence [[p|Proxy|], pure do TH.VarP paramTokenName]
                <*> do TH.NormalB <$> TH.CaseE
                        do TH.VarE paramTokenName
                        <$> traverse
                            do \(tokenName, tokenPat) -> do
                                let tokenLitTy = TH.LitT do TH.StrTyLit tokenName
                                TH.Match
                                    <$> tokenPat
                                    <*> do TH.NormalB <$>
                                            [e|henum (Proxy :: Proxy $(pure tokenLitTy))|]
                                    <*> pure []
                            do tokens
                <*> pure []
