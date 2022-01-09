module Lexer.CodeUnitSpec where

import           Test.Hspec

import           Lexer.CodeUnit

import qualified Data.CharSet                          as CharSet
import qualified Data.CharSet.Unicode                  as UniCharSet
import qualified Data.EnumSet      as EnumSet


spec :: Spec
spec = do
    describe "catFormat" $ do
        it "should be equal formats" $ do
            catFormat `shouldBe` fromCharSet UniCharSet.format

    describe "catUppercaseLetter" $ do
        it "should be equal uppercase letters" $ do
            catUppercaseLetter `shouldBe` fromCharSet UniCharSet.uppercaseLetter

    describe "catModifierLetter" $ do
        it "should be equal modifier letters" $ do
            catModifierLetter `shouldBe` fromCharSet UniCharSet.modifierLetter

    describe "catLowercaseLetter" $ do
        it "should be equal lowercase letters" $ do
            catLowercaseLetter `shouldBe` fromCharSet UniCharSet.lowercaseLetter

    describe "catTitlecaseLetter" $ do
        it "should be equal titlecase letters" $ do
            catTitlecaseLetter `shouldBe` fromCharSet UniCharSet.titlecaseLetter

    describe "catOtherLetter" $ do
        it "should be equal other letters" $ do
            catOtherLetter `shouldBe` fromCharSet UniCharSet.otherLetter

    describe "catMark" $ do
        it "should be equal marks" $ do
            catMark `shouldBe` fromCharSet UniCharSet.mark

    describe "catDecimalNumber" $ do
        it "should be equal decimal numbers" $ do
            catDecimalNumber `shouldBe` fromCharSet UniCharSet.decimalNumber

    describe "catLetterNumber" $ do
        it "should be equal letter numbers" $ do
            catLetterNumber `shouldBe` fromCharSet UniCharSet.letterNumber

    describe "catOtherNumber" $ do
        it "should be equal other numbers" $ do
            catOtherNumber `shouldBe` fromCharSet UniCharSet.otherNumber

    describe "catClosePunctuation" $ do
        it "should be equal close punctuations" $ do
            catClosePunctuation `shouldBe` fromCharSet UniCharSet.closePunctuation

    describe "catConnectorPunctuation" $ do
        it "should be equal connector punctuations" $ do
            catConnectorPunctuation `shouldBe` fromCharSet UniCharSet.connectorPunctuation

    describe "catDashPunctuation" $ do
        it "should be equal dash punctuations" $ do
            catDashPunctuation `shouldBe` fromCharSet UniCharSet.dashPunctuation

    describe "catFinalPunctuation" $ do
        it "should be equal final punctuations" $ do
            catFinalPunctuation `shouldBe` fromCharSet UniCharSet.finalQuote

    describe "catInitialPunctuation" $ do
        it "should be equal initial punctuations" $ do
            catInitialPunctuation `shouldBe` fromCharSet UniCharSet.initialQuote

    describe "catOpenPunctuation" $ do
        it "should be equal open punctuations" $ do
            catOpenPunctuation `shouldBe` fromCharSet UniCharSet.openPunctuation

    describe "catOtherPunctuation" $ do
        it "should be equal other punctuations" $ do
            catOtherPunctuation `shouldBe` fromCharSet UniCharSet.otherPunctuation

    describe "catPunctuation" $ do
        it "should be equal punctuations" $ do
            catPunctuation `shouldBe` fromCharSet UniCharSet.punctuation

    describe "catSymbol" $ do
        it "should be equal symbols" $ do
            catSymbol `shouldBe` fromCharSet UniCharSet.symbol

    describe "catParagraphSeparator" $ do
        it "should be equal paragraph separators" $ do
            catParagraphSeparator `shouldBe` fromCharSet UniCharSet.paragraphSeparator

    describe "catLineSeparator" $ do
        it "should be equal line separators" $ do
            catLineSeparator `shouldBe` fromCharSet UniCharSet.lineSeparator

    describe "catSpaceSeparator" $ do
        it "should be equal space separators" $ do
            catSpaceSeparator `shouldBe` fromCharSet UniCharSet.space

fromCharSet :: CharSet.CharSet -> EnumSet.EnumSet CodeUnit
fromCharSet cs = CharSet.fold go EnumSet.empty cs where
    go c s = EnumSet.insert
        (fromChar c)
        s
