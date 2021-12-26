module Types where

import           Data.ByteString (ByteString)

data WsToken
    = WsTokWhitespace
    | WsTokLineComment
    | WsTokOpenComment
    | WsTokCloseComment
    | WsTokEnclosedComment
    deriving (Eq, Show)

data Token
    = TokSpOpenParen
    | TokSpCloseParen
    | TokSpComma
    | TokSpSemicolon
    | TokSpOpenBracket
    | TokSpCloseBracket
    | TokSpBacktick
    | TokSpOpenBrace
    | TokSpCloseBrace
    | TokSpOpenVirtualBrace
    | TokSpCloseVirtualBrace
    | TokSpVirtualSemicolon
    | TokKwCase
    | TokKwClass
    | TokKwData
    | TokKwDefault
    | TokKwDeriving
    | TokKwDo
    | TokKwElse
    | TokKwForeign
    | TokKwIf
    | TokKwImport
    | TokKwIn
    | TokKwInfix
    | TokKwInfixl
    | TokKwInfixr
    | TokKwInstance
    | TokKwLet
    | TokKwModule
    | TokKwNewtype
    | TokKwOf
    | TokKwThen
    | TokKwType
    | TokKwWhere
    | TokKwUnderscore
    | TokSymDots
    | TokSymColon
    | TokSymDoubleColon
    | TokSymEqual
    | TokSymBackslash
    | TokSymBar
    | TokSymLeftArrow
    | TokSymRightArrow
    | TokSymAt
    | TokSymTilde
    | TokSymRightDoubleArrow
    | TokQualifiedVarId [ByteString] ByteString
    | TokQualifiedConId [ByteString] ByteString
    | TokQualifiedVarSym [ByteString] ByteString
    | TokQualifiedConSym [ByteString] ByteString
    | TokLitInteger ByteString
    | TokLitFloat ByteString
    | TokLitChar ByteString
    | TokLitString ByteString
    deriving (Eq, Show)

data Program = Program
    {
        moduleId :: Maybe QualifiedId,
        exports :: [Export],
        body :: ProgramBody
    }
    deriving (Eq, Show)

data ProgramBody = ProgramBody
    {
        importDecls :: [ImportDecl],
        topDecls :: [Decl]
    }
    deriving (Eq, Show)

data Export = Export
    deriving (Eq, Show)

data ImportDecl = ImportDecl
    deriving (Eq, Show)

data Decl = Decl
    deriving (Eq, Show)

data QualifiedId = QualifiedId [ByteString] ByteString
    deriving (Eq, Show)
