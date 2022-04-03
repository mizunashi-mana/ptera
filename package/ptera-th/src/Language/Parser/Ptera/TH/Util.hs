{-# LANGUAGE TemplateHaskell #-}

module Language.Parser.Ptera.TH.Util (
    genGrammarToken,
    GenRulesTypes (..),
    genRules,
    genParsePoints,
    module Language.Parser.Ptera.Data.HEnum,
    unsafeMembership,
) where

import           Language.Parser.Ptera.Prelude

import qualified Language.Haskell.TH              as TH
import qualified Language.Haskell.TH.Syntax       as TH
import           Language.Parser.Ptera.Data.HEnum (HEnum (..))
import           Language.Parser.Ptera.TH.Syntax
import           Prelude                          (String)
import qualified Type.Membership                  as Membership
import qualified Unsafe.Coerce                    as Unsafe

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
                        <$> buildTokenToTerminalMatchesQ
                            do 0 :: Int
                            do []
                            do tokens
                <*> pure []

        buildTokensMemberInstDsQ n ds = \case
            [] ->
                pure ds
            (tokenName, _):ts -> do
                let tokenLitTy = TH.LitT do TH.StrTyLit tokenName
                tokenDs <- [d|
                    instance TokensMember $(tokensTy) $(pure tokenLitTy) where
                        tokensMembership _ = unsafeMembership $(TH.lift n)
                    |]
                buildTokensMemberInstDsQ
                    do n + 1
                    do tokenDs ++ ds
                    do ts

        buildTokenToTerminalMatchesQ n ms = \case
            [] ->
                pure ms
            (_, tokenPat):ts -> do
                m <- TH.Match
                    <$> tokenPat
                    <*> do TH.NormalB <$>
                            [e|UnsafeHEnum $(TH.lift n)|]
                    <*> pure []
                buildTokenToTerminalMatchesQ
                    do n + 1
                    do m:ms
                    do ts

        tokensTy = pure do TH.ConT tyName

data GenRulesTypes = GenRulesTypes
    { genRulesCtxTy    :: TH.Q TH.Type
    , genRulesTokensTy :: TH.Q TH.Type
    , genRulesTokenTy  :: TH.Q TH.Type
    }

genRules :: TH.Name -> GenRulesTypes -> [(TH.Name, String, TH.Q TH.Type)] -> TH.Q [TH.Dec]
genRules rulesTyName genRulesTypes ruleDefs
    = (:) <$> rulesTyD <*>
        do (:) <$> rulesTagInstD <*>
            do (:) <$> ruleExprTypeTyD <*>
                do (++) <$> rulesInstD <*> hasFieldDs
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

        rulesInstD =
            [d|
            instance Rules $(rulesTy) where
                generateRules =
                    $(foldl'
                        do \l _ -> [|HFCons DictF $(l)|]
                        do [|HFNil|]
                        ruleDefs
                    )
            |]

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
                    instance HasRuleExprField $(rulesTy) $(nameTy) where
                        type RuleExprReturnType $(rulesTy) $(nameTy) = $(ty)

                        getExprField x _ = $(pure do TH.VarE fieldName) x
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

genParsePoints :: TH.Name -> TH.Name -> [String] -> TH.Q [TH.Dec]
genParsePoints tyName rulesTyName initials = (:) <$> parsePointsTyD <*> memberInitialsInstD where
    parsePointsTyD = TH.TySynD tyName [] <$> buildParsePointsSymList initials

    memberInitialsInstD =
        [d|
        instance MemberInitials $(rulesTy) $(parsePointsTy) where
            memberInitials =
                $(foldl'
                    do \l _ -> [|HFCons DictF $(l)|]
                    do [|HFNil|]
                    do initials
                )
        |]

    buildParsePointsSymList = \case
        [] ->
            [t|'[]|]
        n:ns -> do
            let nameTy = TH.LitT do TH.StrTyLit n
            [t|$(pure nameTy) ': $(buildParsePointsSymList ns)|]

    parsePointsTy = pure do TH.ConT tyName

    rulesTy = pure do TH.ConT rulesTyName

unsafeMembership :: Int -> Membership.Membership xs x
unsafeMembership = Unsafe.unsafeCoerce
