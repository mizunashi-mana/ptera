{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Data.Text (Text)
import qualified Data.Text as Text
import Language.Parser.Ptera.TH (LiftType (..))


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

instance LiftType Token where
    liftType _ = [t|Token|]

data Location = Location
    { locRow :: Int
    , locCol :: Int
    , locAbs :: Int
    }
    deriving (Eq, Show)

data Program = Program
    {
        progModuleId :: Maybe QualifiedId,
        progExportItems :: [ExportItem],
        progBody     :: ProgramBody
    }
    deriving (Eq, Show)

instance LiftType Program where
    liftType _ = [t|Program|]

data ProgramBody = ProgramBody
    {
        importDecls :: [ImportDecl],
        topDecls    :: [Decl]
    }
    deriving (Eq, Show)

instance LiftType ProgramBody where
    liftType _ = [t|ProgramBody|]

data ExportItem
    = ExportItemId QualifiedId
    | ExportItemTyConAll QualifiedId
    | ExportItemTyConSpecified QualifiedId [Id]
    | ExportItemModule QualifiedId
    deriving (Eq, Show)

instance LiftType ExportItem where
    liftType _ = [t|ExportItem|]

data ImportDecl = ImportDecl
    {
        importDeclQualified :: Bool,
        importDeclModId     :: QualifiedId,
        importDeclAs        :: Maybe QualifiedId,
        importDeclSpec      :: Maybe ImportSpec
    }
    deriving (Eq, Show)

instance LiftType ImportDecl where
    liftType _ = [t|ImportDecl|]

data ImportSpec
    = ImportSpecSpecified [ImportItem]
    | ImportSpecHiding [ImportItem]
    deriving (Eq, Show)

instance LiftType ImportSpec where
    liftType _ = [t|ImportSpec|]

data ImportItem
    = ImportItemId Id
    | ImportItemTyConAll Id
    | ImportItemTyConSpecified Id [Id]
    deriving (Eq, Show)

instance LiftType ImportItem where
    liftType _ = [t|ImportItem|]

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

instance LiftType Decl where
    liftType _ = [t|Decl|]

data Fixity
    = FixityInfixL
    | FixityInfixR
    | FixityInfix
    deriving (Eq, Show)

instance LiftType Fixity where
    liftType _ = [t|Fixity|]

data Rhs = Rhs [([Guard], Exp)] [Decl]
    deriving (Eq, Show)

instance LiftType Rhs where
    liftType _ = [t|Rhs|]

data Guard
    = GuardPat Pat Exp
    | GuardLet [Decl]
    | GuardExp Exp
    deriving (Eq, Show)

instance LiftType Guard where
    liftType _ = [t|Guard|]

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

instance LiftType Exp where
    liftType _ = [t|Exp|]

data CaseAlt = CaseAlt Pat [([Guard], Exp)] [Decl]
    deriving (Eq, Show)

instance LiftType CaseAlt where
    liftType _ = [t|CaseAlt|]

data Stmt
    = StmtExp Exp
    | StmtPat Pat Exp
    | StmtLet [Decl]
    deriving (Eq, Show)

instance LiftType Stmt where
    liftType _ = [t|Stmt|]

data ForeignCallConv
    = ForeignCallCcall
    | ForeignCallStdcall
    | ForeignCallCplusplus
    | ForeignCallJvm
    | ForeignCallDotnet
    deriving (Eq, Show)

instance LiftType ForeignCallConv where
    liftType _ = [t|ForeignCallConv|]

data Safety
    = Unsafe
    | Safe
    deriving (Eq, Show)

instance LiftType Safety where
    liftType _ = [t|Safety|]

data Type
    = TypeArrow Type Type
    | TypeApp Type [Type]
    | TypeId QualifiedId
    | TypeTupleCon Int
    | TypeTuple [Type]
    | TypeList Type
    deriving (Eq, Show)

instance LiftType Type where
    liftType _ = [t|Type|]

newtype Context = Context [Type]
    deriving (Eq, Show)

instance LiftType Context where
    liftType _ = [t|Context|]

data Constr
    = ConstrWithFields Id [(Strictness, [Id], Type)]
    | ConstrApp Id [(Strictness, Type)]
    deriving (Eq, Show)

instance LiftType Constr where
    liftType _ = [t|Constr|]

data Strictness
    = Unstrict
    | Strict
    deriving (Eq, Show)

instance LiftType Strictness where
    liftType _ = [t|Strictness|]

newtype Deriving = Deriving [Type]
    deriving (Eq, Show)

instance LiftType Deriving where
    liftType _ = [t|Deriving|]

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

instance LiftType Pat where
    liftType _ = [t|Pat|]

data Gcon
    = GconId QualifiedId
    | GconTuple Int
    deriving (Eq, Show)

instance LiftType Gcon where
    liftType _ = [t|Gcon|]

data Lit
    = LitInteger Integer
    | LitFloat Rational
    | LitChar Char
    | LitString String
    deriving (Eq, Show)

instance LiftType Lit where
    liftType _ = [t|Lit|]

newtype Id = Id Text
    deriving (Eq, Show)

instance LiftType Id where
    liftType _ = [t|Id|]

mkId :: String -> Id
mkId s = Id (Text.pack s)

newtype ModId = ModId [Id]
    deriving (Eq, Show)

data QualifiedId = QualifiedId ModId Id
    deriving (Eq, Show)

instance LiftType QualifiedId where
    liftType _ = [t|QualifiedId|]

nonQualifiedId :: Id -> QualifiedId
nonQualifiedId x = QualifiedId (ModId []) x
