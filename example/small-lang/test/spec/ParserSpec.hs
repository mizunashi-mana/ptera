module ParserSpec (spec) where

import           Parser
import           Test.Hspec

import qualified Data.ByteString.Char8 as Char8
import           Data.Either           (isLeft)
import           Types

spec :: Spec
spec = do
    describe "parseExpr" $ do
        it "parse valid programs" $ do
            parseExpr
                [ TokParenOpen
                , TokIdentifier $ Char8.pack "x1"
                , TokPlus
                , TokLitInteger 200
                , TokMulti
                , TokIdentifier $ Char8.pack "z"
                , TokParenClose
                , TokMulti
                , TokParenOpen
                , TokLitInteger 1
                , TokPlus
                , TokLitInteger 2
                , TokParenClose
                , TokEndOfInput
                ]
                `shouldBe`
                    Right
                        (Product
                            (Sum (Var  $ Char8.pack "x1") (Product (Value 200) (Var $ Char8.pack "z")))
                            (Sum (Value 1) (Value 2))
                            )

        it "failed to parse invalid programs" $ do
            parseExpr
                [ TokParenOpen
                , TokIdentifier $ Char8.pack "x1"
                , TokPlus
                , TokLitInteger 200
                , TokEndOfInput
                ]
                `shouldSatisfy` isLeft
            parseExpr
                [ TokParenOpen
                , TokIdentifier $ Char8.pack "x1"
                , TokPlus
                , TokLitInteger 200
                , TokParenClose
                , TokPlus
                , TokEndOfInput
                ]
                `shouldSatisfy` isLeft
            parseExpr
                [ TokParenOpen
                , TokIdentifier $ Char8.pack "x1"
                , TokPlus
                , TokLitInteger 200
                , TokMulti
                , TokIdentifier $ Char8.pack "z"
                , TokParenClose
                , TokMulti
                , TokParenOpen
                , TokLitInteger 1
                , TokPlus
                , TokLitInteger 2
                , TokParenClose
                ] `shouldSatisfy` isLeft
