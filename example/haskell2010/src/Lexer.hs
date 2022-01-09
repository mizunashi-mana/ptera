{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Lexer where

import qualified Data.ByteString     as ByteString
import qualified GHC.Word            as Word
import qualified Language.Lexer.Tlex as Tlex
import qualified Lexer.Rules         as LexerRules
import           Types

import qualified Debug.Trace as Debug


$(LexerRules.buildLexer)

data LexerContext = LexerContext
    { commentNestLevel :: Int
    , restString       :: [Word.Word8]
    }
    deriving (Eq, Show)

lexByteString :: ByteString.ByteString -> Either String [Token]
lexByteString input = go initialLctx id where
    initialLctx = LexerContext
        { commentNestLevel = 0
        , restString = ByteString.unpack input
        }

    initialState lctx = case commentNestLevel lctx of
        0 -> LexerRules.Initial
        _ -> LexerRules.NestedComment

    go lctx0 acc0 =
        let istate = initialState lctx0
            s = restString lctx0
        in case Tlex.runInputString (tlexScan istate) s of
            (Tlex.TlexEndOfInput, _)      -> Right $ acc0 []
            (Tlex.TlexError, ctx)         -> Left $ show (ctx, acc0 [])
            (Tlex.TlexAccepted ctx mact, _) ->
                let consumed = Tlex.inputStringCtxPos ctx
                    consumedString = ByteString.pack $ take consumed s
                    (lctx1, acc1) = case mact of
                        Left wsTok ->
                            ( LexerContext
                                { commentNestLevel = case wsTok of
                                    WsTokOpenComment  -> commentNestLevel lctx0 + 1
                                    WsTokCloseComment -> commentNestLevel lctx0 - 1
                                    _                 -> commentNestLevel lctx0
                                , restString = Tlex.inputStringCtxRest ctx
                                }
                            , acc0
                            )
                        Right act ->
                            ( lctx0
                                { restString = Tlex.inputStringCtxRest ctx
                                }
                            , \n -> acc0 $ act consumedString : n
                            )
                in go lctx1 acc1
