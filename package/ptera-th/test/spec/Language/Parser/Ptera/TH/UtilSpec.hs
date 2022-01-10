{-# LANGUAGE TemplateHaskell #-}

module Language.Parser.Ptera.TH.UtilSpec where

import           Language.Parser.Ptera.Prelude
import           Test.Hspec

import Language.Parser.Ptera.TH
import qualified Type.Membership as Membership
import qualified Type.Membership.Internal as MembershipInternal
import qualified Language.Haskell.TH as TH

$(genGrammarToken (TH.mkName "Tokens") [t|Int|]
    [ ("+", [p|0|])
    , ("*", [p|1|])
    , ("(", [p|2|])
    , (")", [p|3|])
    , ("0", [p|_|])
    ])

spec :: Spec
spec = do
    describe "tokensMembership" do
        it "returns correct membership" do
            Membership.getMemberId (tokensMembership do proxy# @'(Tokens, "*"))
                `shouldBe` Membership.getMemberId do correctMembership @"*"
            Membership.getMemberId (tokensMembership do proxy# @'(Tokens, ")"))
                `shouldBe` Membership.getMemberId do correctMembership @")"

correctMembership :: forall t. MembershipInternal.Member (TokensTag Tokens) t
    => Membership.Membership (TokensTag Tokens) t
correctMembership = MembershipInternal.membership @(TokensTag Tokens) @t
