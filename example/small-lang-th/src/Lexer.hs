{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Lexer where

import qualified Data.ByteString     as ByteString
import qualified GHC.Word
import qualified Language.Lexer.Tlex as Tlex
import qualified Lexer.Rules
import           Types


$(Lexer.Rules.buildLexer)

lexByteString :: ByteString.ByteString -> Either String [Token]
lexByteString input = go (ByteString.unpack input) id where
    go s acc = case Tlex.runInputString (tlexScan ()) s of
        (Tlex.TlexEndOfInput, _) ->
            Right $ acc [TokEndOfInput]
        (Tlex.TlexError, ctx) ->
            Left $ show (ctx, acc [])
        (Tlex.TlexAccepted ctx mact, _) -> do
            let rest = Tlex.inputStringCtxRest ctx
            case mact of
                Nothing ->
                    go rest acc
                Just act -> do
                    let consumedLen = Tlex.inputStringCtxPos ctx
                    let consumed = ByteString.pack $ take consumedLen s
                    go rest (\n -> acc (act consumed:n))
