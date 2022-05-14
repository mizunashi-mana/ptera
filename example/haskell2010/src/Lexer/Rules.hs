{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Lexer.Rules where

import qualified Data.EnumSet                  as EnumSet
import qualified Data.Text                     as Text
import qualified Language.Haskell.TH           as TH
import qualified Language.Lexer.Tlex           as Tlex
import qualified Language.Lexer.Tlex.Plugin.TH as TlexTH
import qualified Lexer.CodeUnit                as CodeUnit
import           Types

data LexerState
    = Initial
    | NestedComment
    deriving (Eq, Show, Enum)

type LexerAction = Either WsToken (Text.Text -> Token)
type LexerCodeUnit = CodeUnit.CodeUnit

type ScannerBuilder = TlexTH.THScannerBuilder LexerState LexerCodeUnit LexerAction
type Pattern = Tlex.Pattern LexerCodeUnit
type CharSet = EnumSet.EnumSet LexerCodeUnit

initialRule :: Pattern -> TH.Q (TH.TExp LexerAction) -> ScannerBuilder ()
initialRule = TlexTH.thLexRule [Initial]

nestedCommentRule :: Pattern -> TH.Q (TH.TExp LexerAction) -> ScannerBuilder ()
nestedCommentRule = TlexTH.thLexRule [NestedComment]

buildLexer :: TH.Q [TH.Dec]
buildLexer = do
    stateTy <- [t|LexerState|]
    codeUnitTy <- [t|LexerCodeUnit|]
    actionTy <- [t|LexerAction|]
    let lexer = TlexTH.buildTHScanner codeUnitTy stateTy actionTy lexerRules
    TlexTH.outputScanner lexer

lexerRules :: ScannerBuilder ()
lexerRules = do
    initialRule (Tlex.someP whitecharP) [||Left WsTokWhitespace||]

    initialRule commentP [||Left WsTokLineComment||]

    -- openComP should be the head on nested comment mode to avoid conflicting.
    TlexTH.thLexRule [Initial, NestedComment] openComP [||Left WsTokOpenComment||]
    -- closeComP should be the head on nested comment mode to avoid conflicting.
    nestedCommentRule closeComP [||Left WsTokCloseComment||]
    nestedCommentRule anyWithNewlineP [||Left WsTokEnclosedComment||]

    specialRule

    -- reservedIdP should be before qvarid to avoid conflicting.
    reservedIdRule
    -- reservedOpP should be before qvarsym / qconsym to avoid conflicting.
    reservedOpRule

    qualifiedIdentifierRule

    initialRule litIntegerP [||Right TokLitInteger||]
    initialRule litFloatP [||Right TokLitFloat||]
    initialRule litCharP [||Right TokLitChar||]
    initialRule litStringP [||Right TokLitString||]

-- See https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-160002.2
-- See also, https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-8.10.2-release/compiler/parser/Lexer.x#L2136

specialRule = do
    initialRule (chP '(') [||Right $ \_ -> TokSpOpenParen||]
    initialRule (chP ')') [||Right $ \_ -> TokSpCloseParen||]
    initialRule (chP ',') [||Right $ \_ -> TokSpComma||]
    initialRule (chP ';') [||Right $ \_ -> TokSpSemicolon||]
    initialRule (chP '[') [||Right $ \_ -> TokSpOpenBracket||]
    initialRule (chP ']') [||Right $ \_ -> TokSpCloseBracket||]
    initialRule (chP '`') [||Right $ \_ -> TokSpBacktick||]
    initialRule (chP '{') [||Right $ \_ -> TokSpOpenBrace||]
    initialRule (chP '}') [||Right $ \_ -> TokSpCloseBrace||]
specialCs = charsCs
    ['(', ')', ',', ';', '[', ']', '`', '{', '}']

whitecharP = Tlex.orP
    [ newlineP
    , vertabP
    , spaceP
    , tabP
    , uniWhiteP
    ]
newlineP = Tlex.orP
    [ returnP <> lineFeedP
    , returnP
    , lineFeedP
    , formFeedP
    ]
newlineCs = mconcat
    [ returnCs
    , lineFeedCs
    , formFeedCs
    ]
returnP = charSetP returnCs
returnCs = charsCs ['\r']
lineFeedP = charSetP lineFeedCs
lineFeedCs = charsCs ['\n']
formFeedP = charSetP formFeedCs
formFeedCs = charsCs ['\f']
vertabP = charSetP vertabCs
vertabCs = charsCs ['\v']
spaceP = charSetP spaceCs
spaceCs = charsCs [' ']
tabP = charSetP tabCs
tabCs = charsCs ['\t']
uniWhiteP = charSetP uniWhiteCs
uniWhiteCs = mconcat
    [ charsCs ['\x200E', '\x200F']
    , CodeUnit.catSpaceSeparator
    ]

commentP = dashesP <> Tlex.maybeP (anyWithoutSymbolP <> Tlex.manyP anyP) <> newlineP where
    anyWithoutSymbolP = charSetP $ anyCs `csDifference` symbolCs
dashesP = dashP <> dashP <> Tlex.manyP dashP
dashP = chP '-'
openComP = stringP "{-"
closeComP = stringP "-}"

anyWithNewlineP = Tlex.orP
    [ graphicP
    , whitecharP
    ]
anyP = charSetP anyCs
anyCs = mconcat
    [ graphicCs
    , spaceCs
    , tabCs
    ]
graphicP = charSetP graphicCs
graphicCs = mconcat
    [ smallCs
    , largeCs
    , symbolCs
    , digitCs
    , specialCs
    , charsCs ['"', '\'']
    ]

smallP = charSetP smallCs
smallCs = mconcat
    [ ascSmallCs
    , uniSmallCs
    , charsCs ['_']
    ]
ascSmallCs = charsCs ['a'..'z']
uniSmallCs = mconcat
    [ CodeUnit.catLowercaseLetter
    , CodeUnit.catOtherLetter
    ]

largeP = charSetP largeCs
largeCs = mconcat
    [ ascLargeCs
    , uniLargeCs
    ]
ascLargeP = charSetP ascLargeCs
ascLargeCs = charsCs ['A'..'Z']
uniLargeCs = mconcat
    [ CodeUnit.catUppercaseLetter
    , CodeUnit.catTitlecaseLetter
    ]

symbolP = charSetP symbolCs
symbolCs = mconcat
    [ ascSymbolCs
    , uniSymbolCs
    ]
    `csDifference` mconcat
        [ specialCs
        , charsCs ['_', '"', '\'']
        ]
ascSymbolCs = charsCs
    [ '!', '#', '$', '%', '&', '*', '+', '.', '/', '<', '=', '>'
    , '?', '@', '\\', '^', '|', '-', '~', ':'
    ]
uniSymbolCs = mconcat
    [ CodeUnit.catSymbol
    , CodeUnit.catPunctuation
    ]
digitP = charSetP digitCs
digitCs = mconcat
    [ ascDigitCs
    , uniDigitCs
    ]
ascDigitCs = charsCs ['0'..'9']
uniDigitCs = CodeUnit.catDecimalNumber

octitP = charSetP octitCs
octitCs = charsCs ['0'..'7']
hexitP = charSetP hexitCs
hexitCs = mconcat
    [ digitCs
    , charsCs ['A'..'F']
    , charsCs ['a'..'f']
    ]

varidP = smallP <> Tlex.manyP (Tlex.orP [smallP, largeP, digitP, chP '\''])
conidP = largeP <> Tlex.manyP (Tlex.orP [smallP, largeP, digitP, chP '\''])
reservedIdRule = do
    initialRule (stringP "case")        [||Right $ \_ -> TokKwCase||]
    initialRule (stringP "class")       [||Right $ \_ -> TokKwClass||]
    initialRule (stringP "data")        [||Right $ \_ -> TokKwData||]
    initialRule (stringP "default")     [||Right $ \_ -> TokKwDefault||]
    initialRule (stringP "deriving")    [||Right $ \_ -> TokKwDeriving||]
    initialRule (stringP "do")          [||Right $ \_ -> TokKwDo||]
    initialRule (stringP "else")        [||Right $ \_ -> TokKwElse||]
    initialRule (stringP "foreign")     [||Right $ \_ -> TokKwForeign||]
    initialRule (stringP "if")          [||Right $ \_ -> TokKwIf||]
    initialRule (stringP "import")      [||Right $ \_ -> TokKwImport||]
    initialRule (stringP "in")          [||Right $ \_ -> TokKwIn||]
    initialRule (stringP "infix")       [||Right $ \_ -> TokKwInfix||]
    initialRule (stringP "infixl")      [||Right $ \_ -> TokKwInfixl||]
    initialRule (stringP "infixr")      [||Right $ \_ -> TokKwInfixr||]
    initialRule (stringP "instance")    [||Right $ \_ -> TokKwInstance||]
    initialRule (stringP "let")         [||Right $ \_ -> TokKwLet||]
    initialRule (stringP "module")      [||Right $ \_ -> TokKwModule||]
    initialRule (stringP "newtype")     [||Right $ \_ -> TokKwNewtype||]
    initialRule (stringP "of")          [||Right $ \_ -> TokKwOf||]
    initialRule (stringP "then")        [||Right $ \_ -> TokKwThen||]
    initialRule (stringP "type")        [||Right $ \_ -> TokKwType||]
    initialRule (stringP "where")       [||Right $ \_ -> TokKwWhere||]
    initialRule (stringP "_")           [||Right $ \_ -> TokKwUnderscore||]

varsymP = symbolWithoutColonP <> Tlex.manyP symbolP where
    symbolWithoutColonP = charSetP (symbolCs `csDifference` charsCs [':'])
consymP = chP ':' <> Tlex.manyP symbolP
reservedOpRule = do
    initialRule (stringP "..")      [||Right $ \_ -> TokSymDots||]
    initialRule (stringP ":")       [||Right $ \_ -> TokSymColon||]
    initialRule (stringP "::")      [||Right $ \_ -> TokSymDoubleColon||]
    initialRule (stringP "=")       [||Right $ \_ -> TokSymEqual||]
    initialRule (stringP "\\")      [||Right $ \_ -> TokSymBackslash||]
    initialRule (stringP "|")       [||Right $ \_ -> TokSymBar||]
    initialRule (stringP "<-")      [||Right $ \_ -> TokSymLeftArrow||]
    initialRule (stringP "->")      [||Right $ \_ -> TokSymRightArrow||]
    initialRule (stringP "@")       [||Right $ \_ -> TokSymAt||]
    initialRule (stringP "~")       [||Right $ \_ -> TokSymTilde||]
    initialRule (stringP "=>")      [||Right $ \_ -> TokSymRightDoubleArrow||]

modidP = Tlex.manyP (conidP <> chP '.') <> conidP

qvaridP = Tlex.maybeP (modidP <> chP '.') <> varidP
qconidP = Tlex.maybeP (modidP <> chP '.') <> conidP
qvarsymP = Tlex.maybeP (modidP <> chP '.') <> varsymP
qconsymP = Tlex.maybeP (modidP <> chP '.') <> consymP
qualifiedIdentifierRule = do
    initialRule qvaridP [||Right $$(qualifiedIdBuilder [||TokQualifiedVarId||])||]
    initialRule qconidP [||Right $$(qualifiedIdBuilder [||TokQualifiedConId||])||]
    initialRule qvarsymP [||Right $$(qualifiedIdBuilder [||TokQualifiedVarSym||])||]
    initialRule qconsymP [||Right $$(qualifiedIdBuilder [||TokQualifiedConSym||])||]
    where
        qualifiedIdBuilder n = [||
            \bs ->
                let (q', v') = Text.breakOnEnd (Text.pack ".") bs
                    (q, v) = if Text.null q'
                        then ([], v')
                        else
                            let qWithoutComma = Text.take (Text.length q' - 1) q' in
                            ( Text.split (== '.') qWithoutComma
                            , v'
                            )
                in $$(n) q v
            ||]

decimalP = Tlex.someP digitP
octalP = Tlex.someP octitP
hexadecimalP = Tlex.someP hexitP

litIntegerP = Tlex.orP
    [ decimalP
    , stringP "0o" <> octalP
    , stringP "0O" <> octalP
    , stringP "0x" <> hexitP
    , stringP "0X" <> hexitP
    ]

litFloatP = Tlex.orP
    [ decimalP <> chP '.' <> decimalP <> Tlex.maybeP exponentP
    , decimalP <> exponentP
    ]

exponentP = charsP ['e', 'E'] <> Tlex.maybeP (charsP ['+', '-']) <> decimalP

litCharP = chP '\'' <> Tlex.orP [graphicWithoutSpP, spaceP, charEscapeP] <> chP '\'' where
    graphicWithoutSpP = charSetP
        $ graphicCs `csDifference` charsCs ['\'', '\\']
litStringP = chP '"' <> Tlex.manyP (Tlex.orP [graphicWithoutSpP, spaceP, escapeP, gapP]) <> chP '"' where
    graphicWithoutSpP = charSetP
        $ graphicCs `csDifference` charsCs ['"', '\\']
charEscapeP = escapeBaseP charescWithoutAmpP
escapeP = escapeBaseP charescP
escapeBaseP p = chP '\\' <> Tlex.orP
    [ p
    , asciiP
    , decimalP
    , chP 'o' <> octalP
    , chP 'x' <> hexadecimalP
    ]
charescWithoutAmpP = charsP ['a', 'b', 'f', 'n', 'r', 't', 'v', '\\', '"', '\'']
charescP = Tlex.orP [charescWithoutAmpP, chP '&']
asciiP = Tlex.orP
    [ chP '^' <> cntrlP
    , stringP "NUL"
    , stringP "SOH"
    , stringP "STX"
    , stringP "ETX"
    , stringP "EOT"
    , stringP "ENQ"
    , stringP "ACK"
    , stringP "BEL"
    , stringP "BS"
    , stringP "HT"
    , stringP "LF"
    , stringP "LF"
    , stringP "VT"
    , stringP "FF"
    , stringP "CR"
    , stringP "SO"
    , stringP "SI"
    , stringP "DLE"
    , stringP "DC1"
    , stringP "DC2"
    , stringP "DC3"
    , stringP "DC4"
    , stringP "NAK"
    , stringP "SYN"
    , stringP "ETB"
    , stringP "CAN"
    , stringP "EM"
    , stringP "SUB"
    , stringP "ESC"
    , stringP "FS"
    , stringP "GS"
    , stringP "RS"
    , stringP "US"
    , stringP "SP"
    , stringP "DEL"
    ]
cntrlP = Tlex.orP
    [ ascLargeP
    , chP '@'
    , chP '['
    , chP '\\'
    , chP ']'
    , chP '^'
    , chP '_'
    ]
gapP = chP '\\' <> Tlex.someP whitecharP <> chP '\\'


charsCs :: [Char] -> CharSet
charsCs cs = EnumSet.fromList
    [ e
    | c <- cs
    , let e = case CodeUnit.fromCharPoint c of
            Just x  -> x
            Nothing -> error $ "Unsupported char: " <> show c
    ]

csDifference :: CharSet -> CharSet -> CharSet
csDifference = EnumSet.difference

charSetP :: CharSet -> Pattern
charSetP = Tlex.straightEnumSetP

chP :: Char -> Pattern
chP c = charSetP $ charsCs [c]

charsP :: [Char] -> Pattern
charsP cs = charSetP $ charsCs cs

stringP :: String -> Pattern
stringP s = foldMap chP s

stringsP :: [String] -> Pattern
stringsP ss = Tlex.orP [stringP s | s <- ss]
