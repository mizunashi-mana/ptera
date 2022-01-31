module Types where

import           Data.Text (Text)
import qualified Data.Text as Text


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
    | TokQualifiedVarId [Text] Text
    | TokQualifiedConId [Text] Text
    | TokQualifiedVarSym [Text] Text
    | TokQualifiedConSym [Text] Text
    | TokLitInteger Text
    | TokLitFloat Text
    | TokLitChar Text
    | TokLitString Text
    | TokVirtExpBrace Int
    | TokVirtNewline Int
    | TokVirtEndOfInput
    deriving (Eq, Show)

data Location = Location
    { locRow :: Int
    , locCol :: Int
    , locAbs :: Int
    }
    deriving (Eq, Show)

data Program = Program
    {
        moduleId :: Maybe QualifiedId,
        exports  :: [ExportItem],
        body     :: ProgramBody
    }
    deriving (Eq, Show)

data ProgramBody = ProgramBody
    {
        importDecls :: [ImportDecl],
        topDecls    :: [Decl]
    }
    deriving (Eq, Show)

data ExportItem
    = ExportItemId QualifiedId
    | ExportItemTyConAll QualifiedId
    | ExportItemTyConSpecified QualifiedId [Id]
    | ExportItemModule QualifiedId
    deriving (Eq, Show)

data ImportDecl = ImportDecl
    {
        importDeclQualified :: Bool,
        importDeclModId     :: QualifiedId,
        importDeclAs        :: Maybe QualifiedId,
        importDeclSpec      :: Maybe ImportSpec
    }
    deriving (Eq, Show)

data ImportSpec
    = ImportSpecSpecified [ImportItem]
    | ImportSpecHiding [ImportItem]
    deriving (Eq, Show)

data ImportItem
    = ImportItemId Id
    | ImportItemTyConAll Id
    | ImportItemTyConSpecified Id [Id]
    deriving (Eq, Show)

data Decl
    = DeclType Type Type
    | DeclData (Maybe Context) Type [Constr] (Maybe Deriving)
    | DeclNewtype (Maybe Context) Type Constr (Maybe Deriving)
    | DeclClass (Maybe Context) Id Id [Decl]
    | DeclInstance (Maybe Context) QualifiedId Type [Decl]
    | DeclDefault [Type]
    | DeclSig [Id] (Maybe Context) Type
    | DeclFixity Fixity (Maybe Int) [Id]
    | DeclFun Id [Pat] Rhs
    | DeclVar Pat Rhs
    | DeclForeignImport ForeignCallConv (Maybe Safety) (Maybe String) Id Type
    | DeclForeignExport ForeignCallConv (Maybe String) Id Type
    deriving (Eq, Show)

data Fixity
    = FixityInfixL
    | FixityInfixR
    | FixityInfix
    deriving (Eq, Show)

data Rhs = Rhs [([Guard], Exp)] [Decl]
    deriving (Eq, Show)

data Guard
    = GuardPat Pat Exp
    | GuardLet [Decl]
    | GuardExp Exp
    deriving (Eq, Show)

data Exp
    = ExpSig Exp (Maybe Context) Type
    | ExpMinus Exp
    | ExpInfixApp Exp QualifiedId Exp
    | ExpLambda [Pat] Exp
    | ExpLet [Decl] Exp
    | ExpIf Exp Exp Exp
    | ExpCase Exp [CaseAlt]
    | ExpDo [Stmt] Exp
    | ExpApp Exp [Exp]
    | ExpLit Lit
    | ExpTuple [Exp]
    | ExpTupleCon Int
    | ExpList [Exp]
    | ExpListRange Exp (Maybe Exp) (Maybe Exp)
    | ExpListComp Exp [Guard]
    | ExpSection (Maybe Exp) QualifiedId (Maybe Exp)
    | ExpRecordCon QualifiedId [(QualifiedId, Exp)]
    | ExpRecordUpdate Exp [(QualifiedId, Exp)]
    | ExpId QualifiedId
    deriving (Eq, Show)

data CaseAlt = CaseAlt Pat [([Guard], Exp)] [Decl]
    deriving (Eq, Show)

data Stmt
    = StmtExp Exp
    | StmtPat Pat Exp
    | StmtLet [Decl]
    deriving (Eq, Show)

data ForeignCallConv
    = ForeignCallCcall
    | ForeignCallStdcall
    | ForeignCallCplusplus
    | ForeignCallJvm
    | ForeignCallDotnet
    deriving (Eq, Show)

data Safety
    = Unsafe
    | Safe
    deriving (Eq, Show)

data Type
    = TypeArrow Type Type
    | TypeApp Type [Type]
    | TypeId QualifiedId
    | TypeTupleCon Int
    | TypeTuple [Type]
    | TypeList Type
    deriving (Eq, Show)

newtype Context = Context [Type]
    deriving (Eq, Show)

data Constr
    = ConstrWithFields Id [(Strictness, [Id], Type)]
    | ConstrApp Id [(Strictness, Type)]
    deriving (Eq, Show)

data Strictness
    = Unstrict
    | Strict
    deriving (Eq, Show)

newtype Deriving = Deriving [Type]
    deriving (Eq, Show)

data Pat
    = PatInfixApp Pat QualifiedId Pat
    | PatMinusInteger Integer
    | PatMinusFloat Rational
    | PatApp Gcon [Pat]
    | PatId Id (Maybe Pat)
    | PatCon Gcon
    | PatLit Lit
    | PatWildcard
    | PatTuple [Pat]
    | PatList [Pat]
    | PatLazy Pat
    | PatRecord QualifiedId [(QualifiedId, Pat)]
    deriving (Eq, Show)

data Gcon
    = GconId QualifiedId
    | GconTuple Int
    deriving (Eq, Show)

data Lit
    = LitInteger Integer
    | LitFloat Rational
    | LitChar Char
    | LitString String
    deriving (Eq, Show)

newtype Id = Id Text
    deriving (Eq, Show)

mkId :: String -> Id
mkId s = Id (Text.pack s)

newtype ModId = ModId [Id]
    deriving (Eq, Show)

data QualifiedId = QualifiedId ModId Id
    deriving (Eq, Show)

nonQualifiedId :: Id -> QualifiedId
nonQualifiedId x = QualifiedId (ModId []) x
