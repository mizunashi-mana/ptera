{-# LANGUAGE TemplateHaskell #-}

module Language.Parser.Ptera.Util (
    genGrammarToken,
) where

import Language.Parser.Ptera.Prelude
import Prelude (String)
import Language.Parser.Ptera.Syntax (GrammarToken (..))

import qualified Language.Haskell.TH as TH

genGrammarToken :: TH.Name -> TH.Q TH.Type -> [(String, TH.Q TH.Pat)] -> TH.Q [TH.Dec]
genGrammarToken tyName tokenTy tokens = sequence
    [ TH.TySynD tyName [] <$> foldr
        do \(tokenName, _) ty -> do
            let tokenLitTy = TH.LitT do TH.StrTyLit tokenName
            [t|$(pure tokenLitTy) ': $(ty)|]
        do [t|'[]|]
        do tokens
    , TH.InstanceD Nothing []
        <$> [t|GrammarToken $(tokenTy) $(pure do TH.ConT tyName)|]
        <*> do
            paramTokenName <- TH.newName "token"
            [d|
                tokenToTerminal Proxy $(pure do TH.VarP paramTokenName) =
                    $(TH.CaseE
                        do TH.VarE paramTokenName
                        <$> traverse
                            do \(tokenName, tokenPat) -> do
                                let tokenLitTy = TH.LitT do TH.StrTyLit tokenName
                                TH.Match
                                    <$> tokenPat
                                    <*> do TH.NormalB
                                            <$> [e|henum (Proxy :: Proxy $(pure tokenLitTy))|]
                                    <*> pure []
                            do tokens
                    )
                |]
    ]
