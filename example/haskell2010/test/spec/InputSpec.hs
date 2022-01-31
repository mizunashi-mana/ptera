module InputSpec (spec) where

import           Test.Hspec

import           Data.Either  (isRight)
import qualified Data.Text    as Text
import qualified Data.Text.IO as TextIO
import qualified Lexer
import qualified Parser
import           Types

spec :: Spec
spec = do
    describe "input/PreludeText.hs" $ do
        it "is valid program" $ do
            s <- TextIO.readFile "input/PreludeText.hs"
            parseInput s `shouldSatisfy` isRight

    describe "input/Sample001.hs" $ do
        it "is valid program" $ do
            s <- TextIO.readFile "input/Sample001.hs"
            parseInput s `shouldSatisfy` isRight

    describe "input/Sample002.hs" $ do
        it "is valid program" $ do
            s <- TextIO.readFile "input/Sample002.hs"
            parseInput s `shouldSatisfy` isRight

    describe "input/Sample003.hs" $ do
        it "is valid program" $ do
            s <- TextIO.readFile "input/Sample003.hs"
            parseInput s `shouldSatisfy` isRight

    describe "input/Sample004.hs" $ do
        it "is valid program" $ do
            s <- TextIO.readFile "input/Sample004.hs"
            parseInput s `shouldSatisfy` isRight

parseInput :: Text.Text -> Either String Program
parseInput s = do
    toks <- Lexer.lexText s
    Parser.parseModule toks
