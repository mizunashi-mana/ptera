{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lexer where

import qualified Data.Text as Text
import qualified Lexer.CodeUnit as CodeUnit
import qualified GHC.Word            as Word
import qualified Language.Lexer.Tlex as Tlex
import qualified Lexer.Rules         as LexerRules
import           Types
import qualified Control.Monad.Trans.State.Strict as StateT
import qualified Data.EnumSet    as EnumSet


$(LexerRules.buildLexer)

lexText :: Text.Text -> Either String [Token]
lexText input = go initialLctx id where
    initialLctx = LexerContext
        { commentNestLevel = 0
        , currentLexingCtx = LexingContext
            { currentPosition = Position
                { posRow = 0
                , posCol = 0
                , posAbs = 0
                }
            , currentPosCtx = PosCtxNext
            , restString = input
            }
        }

    initialState lctx = case commentNestLevel lctx of
        0 -> LexerRules.Initial
        _ -> LexerRules.NestedComment

    go lctx0 acc0 =
        let istate = initialState lctx0
            lexingCtx0 = currentLexingCtx lctx0
        in case runLexingM (tlexScan istate) lexingCtx0 of
            (Tlex.TlexEndOfInput, _)      ->
                Right $ acc0 []
            (Tlex.TlexError, lexingCtx1)  ->
                Left $ show (lexingCtx1, acc0 [])
            (Tlex.TlexAccepted lexingCtx1 mact, _) ->
                goAccepted lctx0 acc0 lexingCtx0 lexingCtx1 mact

    goAccepted lctx0 acc0 lexingCtx0 lexingCtx1 mact =
        let consumed = currentPosAbs lexingCtx1 - currentPosAbs lexingCtx0
            consumedString = Text.take consumed $ restString lexingCtx0
            (lctx1, acc1) = case mact of
                Left wsTok ->
                    ( LexerContext
                        { commentNestLevel = case wsTok of
                            WsTokOpenComment  -> commentNestLevel lctx0 + 1
                            WsTokCloseComment -> commentNestLevel lctx0 - 1
                            _                 -> commentNestLevel lctx0
                        , currentLexingCtx = lexingCtx1
                        }
                    , acc0
                    )
                Right act ->
                    ( lctx0
                        { currentLexingCtx = lexingCtx1
                        }
                    , \n -> acc0 $ act consumedString : n
                    )
        in go lctx1 acc1

    currentPosAbs ctx = posAbs $ currentPosition ctx

data Position = Position
    { posRow :: Int
    , posCol :: Int
    , posAbs :: Int
    }
    deriving (Eq, Show)

data LexerContext = LexerContext
    { commentNestLevel :: Int
    , currentLexingCtx :: LexingContext
    }
    deriving (Eq, Show)

data LexingContext = LexingContext
    { currentPosition :: Position
    , currentPosCtx :: PositionContext
    , restString :: Text.Text
    }
    deriving (Eq, Show)

data PositionContext
    = PosCtxNext
    | PosCtxReturn
    deriving (Eq, Show)

newtype LexingM a = LexingM
    {
        unLexingM :: StateT.State LexingContext a
    }
    deriving (Functor, Applicative, Monad) via (StateT.State LexingContext)

runLexingM :: LexingM a -> LexingContext -> (a, LexingContext)
runLexingM (LexingM m) = StateT.runState m

instance Tlex.TlexContext LexingContext CodeUnit.CodeUnit LexingM where
    tlexGetInputPart = LexingM $ do
        ctx <- StateT.get
        case Text.uncons $ restString ctx of
            Nothing ->
                pure Nothing
            Just (c, txt) -> do
                let cu = CodeUnit.fromChar c
                StateT.put $ nextLexingCtx ctx cu txt
                pure $ Just cu
        where
            nextLexingCtx ctx cu txt
                | EnumSet.member cu LexerRules.newlineCs = case currentPosCtx ctx of
                    PosCtxNext -> LexingContext
                        { currentPosition = newlinePosition $ currentPosition ctx
                        , currentPosCtx = nextPosCtx cu
                        , restString = txt
                        }
                    PosCtxReturn -> LexingContext
                        { currentPosition = if EnumSet.member cu LexerRules.lineFeedCs
                            then crlfPosition $ currentPosition ctx
                            else newlinePosition $ currentPosition ctx
                        , currentPosCtx = nextPosCtx cu
                        , restString = txt
                        }
                | otherwise = LexingContext
                    { currentPosition = nextColPosition $ currentPosition ctx
                    , currentPosCtx = PosCtxNext
                    , restString = txt
                    }

            nextColPosition pos = pos
                { posCol = posCol pos + 1
                , posAbs = posAbs pos + 1
                }

            newlinePosition pos = Position
                { posRow = posRow pos + 1
                , posCol = 0
                , posAbs = posAbs pos + 1
                }

            crlfPosition pos = Position
                { posRow = posRow pos
                , posCol = 0
                , posAbs = posAbs pos + 1
                }

            nextPosCtx cu
                | EnumSet.member cu LexerRules.returnCs =
                    PosCtxReturn
                | otherwise =
                    PosCtxNext

    tlexGetMark = LexingM StateT.get
