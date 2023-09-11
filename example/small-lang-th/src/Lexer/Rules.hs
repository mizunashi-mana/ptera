{-# LANGUAGE TemplateHaskellQuotes #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Lexer.Rules where

import qualified Data.ByteString.Char8               as Char8
import qualified Data.CharSet                        as CharSet
import qualified Data.Word                           as Word
import qualified Language.Haskell.TH                 as TH
import qualified Language.Lexer.Tlex                 as Tlex
import qualified Language.Lexer.Tlex.Plugin.Encoding as TlexEnc
import qualified Language.Lexer.Tlex.Plugin.TH       as TlexTH
import           Types


type LexerState = ()
type LexerAction = Maybe (Char8.ByteString -> Token)
type LexerCodeUnit = Word.Word8

type ScannerBuilder = TlexTH.THScannerBuilder LexerState LexerCodeUnit LexerAction
type Pattern = Tlex.Pattern LexerCodeUnit

initialRule :: Pattern -> TH.Code TH.Q LexerAction -> ScannerBuilder ()
initialRule = TlexTH.thLexRule [()]

buildLexer :: TH.Q [TH.Dec]
buildLexer = do
    stateTy <- [t|LexerState|]
    codeUnitTy <- [t|LexerCodeUnit|]
    actionTy <- [t|LexerAction|]
    let lexer = TlexTH.buildTHScanner codeUnitTy stateTy actionTy lexerRules
    TlexTH.outputScanner lexer

lexerRules :: ScannerBuilder ()
lexerRules = do
    initialRule (Tlex.someP whitecharP) [||Nothing||]
    initialRule plusP [||Just (\_ -> TokPlus)||]
    initialRule multiP [||Just (\_ -> TokMulti)||]
    initialRule openParenP [||Just (\_ -> TokParenOpen)||]
    initialRule closeParenP [||Just (\_ -> TokParenClose)||]
    initialRule litIntegerP [||
        Just (\s -> case Char8.readInteger s of
            Just (i, _) -> TokLitInteger i
            Nothing     -> error "unreachable"
        )
        ||]
    initialRule identifierP [||Just TokIdentifier||]

whitecharP = charsP
    [ ' '
    , '\t'
    , '\n'
    , '\r'
    ]

plusP = chP '+'

multiP = chP '*'

openParenP = chP '('

closeParenP = chP ')'

identifierP = smallP
    <> Tlex.manyP (Tlex.orP [smallP, largeP, digitP])

litIntegerP = Tlex.someP digitP

smallP = charSetP $ CharSet.range 'a' 'z'
largeP = charSetP $ CharSet.range 'A' 'Z'
digitP = charSetP $ CharSet.range '0' '9'

charSetP :: CharSet.CharSet -> Pattern
charSetP cs = TlexEnc.charSetP TlexEnc.charSetPUtf8 cs

chP :: Char -> Pattern
chP c = TlexEnc.chP TlexEnc.charSetPUtf8 c

charsP :: [Char] -> Pattern
charsP cs = TlexEnc.charsP TlexEnc.charSetPUtf8 cs
