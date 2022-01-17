module ParserSpec (spec) where

import           Parser
import           Test.Hspec

import qualified Data.ByteString.Char8 as Char8
import           Data.Either           (isLeft)
import           Types

spec :: Spec
spec = do
    describe "parseModule" $ do
        it "parse valid programs" $ do
            parseModule
                [ TokVirtExpBrace 1
                , TokQualifiedVarId [] "f"
                , TokSymEqual,TokQualifiedVarId [] "x"
                , TokKwWhere
                , TokVirtExpBrace 5
                , TokQualifiedVarId [] "x"
                , TokSymEqual
                , TokQualifiedVarId [] "y"
                , TokVirtNewline 5
                , TokQualifiedVarId [] "y"
                , TokSymEqual
                , TokVirtNewline 9
                , TokKwLet
                , TokSpOpenBrace
                , TokVirtNewline 13
                , TokSpSemicolon
                , TokQualifiedVarId [] "z"
                , TokSymEqual
                , TokLitInteger "0"
                , TokSpSemicolon
                , TokVirtNewline 9
                , TokSpCloseBrace
                , TokKwIn
                , TokQualifiedVarId [] "z"
                , TokVirtEndOfInput
                ]
                `shouldBe`
                    Right
                        (Program
                            { moduleId = Nothing
                            , exports = []
                            , body = ProgramBody
                                { importDecls = []
                                , topDecls =
                                    [ DeclVar (PatId (Id "f") Nothing)
                                        (Rhs
                                            [([],ExpApp (ExpId (QualifiedId (ModId []) (Id "x"))) [])]
                                            [ DeclVar (PatId (Id "x") Nothing)
                                                (Rhs [([],ExpApp (ExpId (QualifiedId (ModId []) (Id "y"))) [])] [])
                                            , DeclVar (PatId (Id "y") Nothing)
                                                (Rhs
                                                    [ ([],ExpLet
                                                        [ DeclVar (PatId (Id "z") Nothing)
                                                            (Rhs [([],ExpApp (ExpLit (LitInteger 0)) [])] [])
                                                        ]
                                                        (ExpApp (ExpId (QualifiedId (ModId []) (Id "z"))) [])
                                                      )
                                                    ]
                                                    [])
                                            ])
                                    ]
                                }
                            })

        it "failed to parse invalid programs" $ do
            parseModule [TokVirtEndOfInput]
                `shouldSatisfy` isLeft
