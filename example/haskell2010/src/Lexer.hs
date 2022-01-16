{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Lexer where

import qualified Control.Monad.Trans.State.Strict as StateT
import qualified Data.EnumSet                     as EnumSet
import qualified Data.Text                        as Text
import qualified GHC.Word                         as Word
import qualified Language.Lexer.Tlex              as Tlex
import qualified Lexer.CodeUnit                   as CodeUnit
import qualified Lexer.PreprocessLayout           as PreprocessLayout
import qualified Lexer.Rules                      as LexerRules
import           Types


$(LexerRules.buildLexer)

lexText :: Text.Text -> Either String [Token]
lexText input = PreprocessLayout.preprocessLayout <$> go initialLctx [] where
    initialLctx = LexerContext
        { commentNestLevel = 0
        , currentLexingCtx = LexingContext
            { currentLocation = Location
                { locRow = 1
                , locCol = 1
                , locAbs = 0
                }
            , currentLocCtx = LocCtxNext
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
            (Tlex.TlexEndOfInput, _) ->
                Right $ reverse acc0
            (Tlex.TlexError, lexingCtx1) ->
                Left $ show (lexingCtx1, reverse acc0)
            (Tlex.TlexAccepted lexingCtx1 mact, _) ->
                goAccepted lctx0 acc0 lexingCtx0 lexingCtx1 mact

    goAccepted lctx0 acc0 lexingCtx0 lexingCtx1 mact =
        let consumed = currentLocAbs lexingCtx1 - currentLocAbs lexingCtx0
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
                    , (currentLocation lexingCtx0, act consumedString):acc0
                    )
        in go lctx1 acc1

    currentLocAbs ctx = locAbs $ currentLocation ctx

data LexerContext = LexerContext
    { commentNestLevel :: Int
    , currentLexingCtx :: LexingContext
    }
    deriving (Eq, Show)

data LexingContext = LexingContext
    { currentLocation :: Location
    , currentLocCtx   :: LocationContext
    , restString      :: Text.Text
    }
    deriving (Eq, Show)

data LocationContext
    = LocCtxNext
    | LocCtxReturn
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
                | EnumSet.member cu LexerRules.newlineCs = case currentLocCtx ctx of
                    LocCtxNext -> LexingContext
                        { currentLocation = newlineLoc $ currentLocation ctx
                        , currentLocCtx = nextLocCtx cu
                        , restString = txt
                        }
                    LocCtxReturn -> LexingContext
                        { currentLocation = if EnumSet.member cu LexerRules.lineFeedCs
                            then crlfLoc $ currentLocation ctx
                            else newlineLoc $ currentLocation ctx
                        , currentLocCtx = nextLocCtx cu
                        , restString = txt
                        }
                | otherwise = LexingContext
                    { currentLocation = nextColLoc $ currentLocation ctx
                    , currentLocCtx = LocCtxNext
                    , restString = txt
                    }

            nextColLoc loc = loc
                { locCol = locCol loc + 1
                , locAbs = locAbs loc + 1
                }

            newlineLoc loc = Location
                { locRow = locRow loc + 1
                , locCol = 1
                , locAbs = locAbs loc + 1
                }

            crlfLoc loc = Location
                { locRow = locRow loc
                , locCol = 1
                , locAbs = locAbs loc + 1
                }

            nextLocCtx cu
                | EnumSet.member cu LexerRules.returnCs =
                    LocCtxReturn
                | otherwise =
                    LocCtxNext

    tlexGetMark = LexingM StateT.get
