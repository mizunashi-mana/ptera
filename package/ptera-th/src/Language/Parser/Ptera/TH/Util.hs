{-# LANGUAGE TemplateHaskell #-}

module Language.Parser.Ptera.TH.Util (
    genGrammarToken,
    GenRulesTypes (..),
    genRules,
    henumA,
    unsafeMembership,
) where

import           Language.Parser.Ptera.Prelude

import           Language.Parser.Ptera.TH.Syntax
import           Language.Parser.Ptera.Data.HEnum (henumA)
import           Prelude                         (String)
import qualified Language.Haskell.TH             as TH
import qualified Language.Haskell.TH.Syntax             as TH
import qualified Type.Membership as Membership
import qualified Unsafe.Coerce as Unsafe

genGrammarToken :: TH.Name -> TH.Q TH.Type -> [(String, TH.Q TH.Pat)] -> TH.Q [TH.Dec]
genGrammarToken tyName tokenTy tokens = do
    grammarTokenInstD <- grammarTokenInstDQ
    tokensTagInstD <- tokensTagInstDQ
    tokensMemberInstDs <- tokensMemberInstDsQ
    pure do [tokensTyD, grammarTokenInstD, tokensTagInstD] ++ tokensMemberInstDs
    where
        tokensTyD = TH.DataD [] tyName [] Nothing [] []

        grammarTokenInstDQ = TH.InstanceD Nothing []
            <$> [t|GrammarToken $(pure do TH.ConT tyName) $(tokenTy)|]
            <*> sequence
                [ TH.FunD
                    do TH.mkName "tokenToTerminal"
                    <$> sequence [tokenToTerminalClause]
                ]

        tokensTagInstDQ = TH.TySynInstD
            <$> do
                TH.TySynEqn Nothing
                    <$> [t|TokensTag $(tokensTy)|]
                    <*> foldr
                        do \(tokenName, _) ty -> do
                            let tokenLitTy = TH.LitT do TH.StrTyLit tokenName
                            [t|$(pure tokenLitTy) ': $(ty)|]
                        do [t|'[]|]
                        do tokens

        tokensMemberInstDsQ = buildTokensMemberInstDsQ
            do 0 :: Int
            do []
            do tokens

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

        buildTokensMemberInstDsQ n ds ts = case ts of
            [] ->
                pure ds
            (tokenName, _):ts' -> do
                let tokenLitTy = TH.LitT do TH.StrTyLit tokenName
                tokenDs <- [d|
                    instance TokensMember $(tokensTy) $(pure tokenLitTy) where
                        tokensMembership _ = unsafeMembership $(TH.lift n)
                    |]
                buildTokensMemberInstDsQ
                    do n + 1
                    do tokenDs ++ ds
                    do ts'

        tokensTy = pure do TH.ConT tyName

data GenRulesTypes = GenRulesTypes
    { genRulesCtxTy    :: TH.Q TH.Type
    , genRulesTokensTy :: TH.Q TH.Type
    , genRulesTokenTy  :: TH.Q TH.Type
    }

genRules :: TH.Name -> GenRulesTypes -> [(TH.Name, String, TH.Q TH.Type)] -> TH.Q [TH.Dec]
genRules rulesTyName genRulesTypes ruleDefs = do
        ds1 <- sequence [ rulesTyD, rulesTagInstD, ruleExprTypeTyD ]
        ds2 <- hasFieldDs
        pure do ds1 ++ ds2
    where
        rulesTyD = TH.DataD [] rulesTyName [] Nothing
            <$> sequence
                [ TH.RecC rulesTyName
                    <$> buildRuleFields [] ruleDefs
                ]
            <*> pure []

        rulesTagInstD = TH.TySynInstD
            <$> do
                TH.TySynEqn Nothing
                    <$> [t|RulesTag $(rulesTy)|]
                    <*> buildNonTerminalSymList [t|'[]|] ruleDefs

        ruleExprTypeTyD = TH.TySynInstD
            <$> do
                TH.TySynEqn Nothing
                    <$> [t|RuleExprType $(rulesTy)|]
                    <*> ruleExprTy

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
                    instance HasRuleExprField $(rulesTy) $(nameTy) where
                        type RuleExprReturnType $(rulesTy) $(nameTy) = $(ty)
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

unsafeMembership :: Int -> Membership.Membership xs x
unsafeMembership = Unsafe.unsafeCoerce
