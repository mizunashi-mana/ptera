{-# LANGUAGE TemplateHaskell #-}

module Language.Parser.Ptera.TH.Util (
    genGrammarToken,
    GenRulesTypes (..),
    genRules
) where

import Language.Parser.Ptera.Prelude
import Prelude (String)
import Language.Parser.Ptera.TH.Syntax

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
                                            [e|henumA @ $(pure tokenLitTy)|]
                                    <*> pure []
                            do tokens
                <*> pure []

data GenRulesTypes = GenRulesTypes
    { genRulesCtxTy :: TH.Q TH.Type
    , genRulesTokensTy :: TH.Q TH.Type
    , genRulesTokenTy :: TH.Q TH.Type
    }

genRules :: TH.Name -> GenRulesTypes -> [(TH.Name, String, TH.Q TH.Type)] -> TH.Q [TH.Dec]
genRules rulesTyName genRulesTypes ruleDefs = do
        ds1 <- sequence [ rulesTyD, rulesTagD ]
        ds2 <- hasFieldDs
        pure do ds1 ++ ds2
    where
        rulesTyD = TH.DataD [] rulesTyName [] Nothing
            <$> sequence
                [ TH.RecC rulesTyName
                    <$> buildRuleFields [] ruleDefs
                ]
            <*> pure []

        rulesTagD = TH.TySynInstD
            <$> do
                TH.TySynEqn Nothing
                    <$> [t|RulesTag $(rulesTy)|]
                    <*> buildNonTerminalSymList [t|'[]|] ruleDefs

        hasFieldDs = buildHasFieldInstances [] ruleDefs

        buildRuleFields acc = \case
            [] ->
                pure acc
            (fieldName, _, ty):rs -> do
                fieldTy <- [t|$(ruleExprTy) $(ty)|]
                buildRuleFields
                    do (fieldName, fieldBang, fieldTy):acc
                    do rs

        buildNonTerminalSymList acc = \case
            [] ->
                acc
            (_, name, _):rs -> do
                let nameTy = TH.LitT do TH.StrTyLit name
                buildNonTerminalSymList
                    [t|$(pure nameTy) ': $(acc)|] rs

        buildHasFieldInstances acc = \case
            [] ->
                pure acc
            (fieldName, name, ty):rs -> do
                let nameTy = pure do TH.LitT do TH.StrTyLit name
                insts <-
                    [d|
                    instance HasField $(nameTy) $(rulesTy) ($(ruleExprTy) $(ty)) where
                        getField x = $(pure do TH.VarE fieldName) x
                    instance HasRuleExprField
                            (SemActM $(genRulesCtxTy genRulesTypes))
                            $(rulesTy)
                            $(genRulesTokensTy genRulesTypes)
                            $(genRulesTokenTy genRulesTypes)
                            $(nameTy)
                        where {
                            type RuleExprReturnType
                                (SemActM $(genRulesCtxTy genRulesTypes))
                                $(rulesTy)
                                $(genRulesTokensTy genRulesTypes)
                                $(genRulesTokenTy genRulesTypes)
                                $(nameTy)
                                = $(ty)
                        }
                    |]
                buildHasFieldInstances
                    do insts ++ acc
                    do rs

        fieldBang = TH.Bang
            TH.NoSourceUnpackedness
            TH.SourceStrict

        rulesTy = pure do TH.ConT rulesTyName

        ruleExprTy =
            [t|RuleExprM
                $(genRulesCtxTy genRulesTypes)
                $(rulesTy)
                $(genRulesTokensTy genRulesTypes)
                $(genRulesTokenTy genRulesTypes)
            |]
