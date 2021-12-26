{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Parser.Rules where

import           Data.Proxy                        (Proxy (..))
import           Language.Parser.Ptera.Data.HEnum  (henumA)
import           Language.Parser.Ptera.Data.HList  (HList (..))
import qualified Language.Parser.Ptera.Data.Record as Record
import           Language.Parser.Ptera.TH          hiding (RuleExpr, Rules)
import qualified Language.Parser.Ptera.TH          as Ptera
import           Types
import           Data.ByteString (ByteString)


grammar :: Grammar ParsePoints Rules Tokens Token
grammar = fixGrammar $ Record.fromFieldsA $
    Record.field #module    rModule :*
    Record.field #body      rBody :*
    Record.field #impdecls  rImportDecls :*
    Record.field #impdecls1 rImportDecls1 :*
    Record.field #exports   rExports :*
    Record.field #exports1  rExports1 :*
    Record.field #export    rExport :*
    Record.field #impdecl   rImportDecl :*
    Record.field #impspec   rImportSpec :*

    Record.field (Proxy :: Proxy "{")   rLayoutOpen :*
    Record.field (Proxy :: Proxy "}")   rLayoutClose :*
    Record.field (Proxy :: Proxy ";")   rLayoutSemicolon :*

    HNil

type ParsePoints = '[ "expr" ]
type Rules =
    '[
        '("module", Program),
        '("body", ProgramBody),
        '("impdecls", [ImportDecl]),
        '("impdecls1", [ImportDecl]),
        '("exports", [Export]),
        '("exports1", [Export]),
        '("export", Export),
        '("impdecl", ImportDecl),
        '("impspec", ImportSpec),

        '("modid", ByteString),
        '("{", ()),
        '("}", ())
    ]
type Tokens =
    '[
        "(", ")", ",", ";", "[", "]", "`", "{", "}",

        "case", "class", "data", "default", "deriving", "do", "else",
        "foreign", "if", "import", "in", "infix", "infixl",
        "infixr", "instance", "let", "module", "newtype", "of",
        "then", "type", "where", "_",

        "..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>",

        "qvarid", "qconid", "qvarsym", "qconsym",

        "integer", "float", "char", "string"
    ]
type RuleExpr = Ptera.RuleExpr Rules Tokens Token
type Unit = Ptera.Unit Rules Tokens Token

instance GrammarToken Token Tokens where
    tokenToTerminal Proxy token = case token of
        TokSpOpenParen{}            -> henumA @"("
        TokSpCloseParen{}           -> henumA @")"
        TokSpComma{}                -> henumA @","
        TokSpSemicolon{}            -> henumA @";"
        TokSpOpenBracket{}          -> henumA @"["
        TokSpCloseBracket{}         -> henumA @"]"
        TokSpBacktick{}             -> henumA @"`"
        TokSpOpenBrace{}            -> henumA @"{"
        TokSpCloseBrace{}           -> henumA @"}"
        TokSpOpenVirtualBrace{}     -> henumA @"vobrace"
        TokSpCloseVirtualBrace{}    -> henumA @"vcbrace"
        TokSpVirtualSemicolon{}     -> henumA @"vsemi"
        TokKwCase{}                 -> henumA @"case"
        TokKwClass{}                -> henumA @"class"
        TokKwData{}                 -> henumA @"data"
        TokKwDefault{}              -> henumA @"default"
        TokKwDeriving{}             -> henumA @"deriving"
        TokKwDo{}                   -> henumA @"do"
        TokKwElse{}                 -> henumA @"else"
        TokKwForeign{}              -> henumA @"foreign"
        TokKwIf{}                   -> henumA @"if"
        TokKwImport{}               -> henumA @"import"
        TokKwIn{}                   -> henumA @"in"
        TokKwInfix{}                -> henumA @"infix"
        TokKwInfixl{}               -> henumA @"infixl"
        TokKwInfixr{}               -> henumA @"infixr"
        TokKwInstance{}             -> henumA @"instance"
        TokKwLet{}                  -> henumA @"let"
        TokKwModule{}               -> henumA @"module"
        TokKwNewtype{}              -> henumA @"newtype"
        TokKwOf{}                   -> henumA @"of"
        TokKwThen{}                 -> henumA @"then"
        TokKwType{}                 -> henumA @"type"
        TokKwWhere{}                -> henumA @"where"
        TokKwUnderscore{}           -> henumA @"_"
        TokSymDots{}                -> henumA @".."
        TokSymColon{}               -> henumA @":"
        TokSymDoubleColon{}         -> henumA @"::"
        TokSymEqual{}               -> henumA @"="
        TokSymBackslash{}           -> henumA @"\\"
        TokSymBar{}                 -> henumA @"|"
        TokSymLeftArrow{}           -> henumA @"<-"
        TokSymRightArrow{}          -> henumA @"->"
        TokSymAt{}                  -> henumA @"@"
        TokSymTilde{}               -> henumA @"~"
        TokSymRightDoubleArrow{}    -> henumA @"=>"
        TokQualifiedVarId{}         -> henumA @"qvarid"
        TokQualifiedConId{}         -> henumA @"qconid"
        TokQualifiedVarSym{}        -> henumA @"qvarsym"
        TokQualifiedConSym{}        -> henumA @"qconsym"
        TokLitInteger{}             -> henumA @"integer"
        TokLitFloat{}               -> henumA @"float"
        TokLitChar{}                -> henumA @"char"
        TokLitString{}              -> henumA @"string"

rModule :: RuleExpr Program
rModule = ruleExpr
    [ alt $ tokA @"module" <^> varA "modid" <^> varA @"exports" <^> tokA @"where" <^> varA @"body"
        <:> semAct \(_ :* modId :* exports :* _ :* body :* HNil) ->
            [||Program (Just $$(modId)) $$(exports) $$(body)||]
    , alt $ tokA @"module" <^> varA "modid" <^> tokA @"where" <^> varA @"body"
        <:> semAct \(_ :* modId :* _ :* body :* HNil) ->
            [||Program (Just $$(modId)) [] $$(body)||]
    , alt $ varA @"body"
        <:> semAct \(body :* HNil) ->
            [||Program Nothing [] $$(body)||]
    ]

rBody :: RuleExpr ProgramBody
rBody = ruleExpr
    [ alt $ varA @"{" <^> varA @"impdecls" <^> varA @";" <^> varA @"topdecls" <^> varA @"}"
        <:> semAct \(_ :* impdecls :* _ :* topdecls :* _ :* HNil) ->
            [||ProgramBody $$(impdecls) $$(topdecls)||]
    , alt $ varA @"{" <^> varA @"impdecls" <^> varA @"}"
        <:> semAct \(_ :* impdecls :* _ :* HNil) ->
            [||ProgramBody $$(impdecls) []||]
    , alt $ varA @"{" <^> varA @"topdecls" <^> varA @"}"
        <:> semAct \(_ :* topdecls :* _ :* HNil) ->
            [||ProgramBody [] $$(topdecls)||]
    ]

rImportDecls :: RuleExpr [ImportDecl]
rImportDecls = ruleExpr
    [ alt $ varA @"impdecl" <^> varA @"impdecls1"
        <:> semAct \(impdecl :* impdecls :* HNil) ->
            [||$$(impdecl):$$(impdecls)||]
    ]

rImportDecls1 :: RuleExpr [ImportDecl]
rImportDecls1 = ruleExpr
    [ alt $ varA @";" <^> varA @"impdecl" <^> varA @"impdecls1"
        <:> semAct \(_ :* impdecl :* impdecls :* HNil) ->
            [||$$(impdecl):$$(impdecls)||]
    , alt $ eps
        $ semAct \HNil -> [||[]||]
    ]

rExports :: RuleExpr [Export]
rExports = ruleExpr
    [ alt $ tokA @"(" <^> varA @"exports1" <^> tokA @")"
        <:> semAct \(_ :* exports :* _ :* HNil) ->
            [||$$(exports)||]
    ]

rExports1 :: RuleExpr [Export]
rExports1 = ruleExpr
    [ alt $ varA @"export" <^> tokA @"," <^> varA @"exports1"
        <:> semAct \(export :* _ :* exports :* HNil) ->
            [||$$(export):$$(exports)||]
    , alt $ varA @"export"
        <:> semAct \(export :* HNil) ->
            [||[$$(export)]||]
    , alt $ eps
        $ semAct \HNil -> [||[]||]
    ]

rExport :: RuleExpr Export
rExport = ruleExpr
    [ alt $ varA @"qvar"
        <:> semAct \(qvar :* HNil) -> [||ExportVar $$(qvar)||]
    , alt $ varA @"qtycon" <^> tokA "(" <^> tokA ".." <^> tokA ")"
    ]

rModuleId :: RuleExpr QualifiedId
rModuleId = ruleExpr
    [ alt $ tokA @"qconid"
        <:> semAct \(name :* HNil) -> [||
            case $$(name) of
                TokQualifiedConId q v -> Just $ QualifiedId q v
                _                     -> error "unreachable: expected qualified conid"
        ||]
    ]

rQualifiedTyCon :: RuleExpr QualifiedId
rQualifiedTyCon = ruleExpr
    [ alt $ tokA "qconid"
        <:> semAct \(name :* HNil) -> [||
            case $$(name) of
                TokQualifiedConId q v -> Just $ QualifiedId q v
                _                     -> error "unreachable: expected qualified conid"
        ||]
    ]

rLayoutOpen :: RuleExpr ()
rLayoutOpen = ruleExpr
    [ alt $ tokA @"{"
        <:> semAct \_ -> [||()||]
    , alt $ tokA @"vobrace"
        <:> semAct \_ -> [||()||]
    ]

rLayoutClose :: RuleExpr ()
rLayoutClose = ruleExpr
    [ alt $ tokA @"}"
        <:> semAct \(_ :* HNil) -> [||()||]
    , alt $ tokA @"vcbrace"
        <:> semAct \(_ :* HNil) -> [||()||]
    , alt $ eps
        $ semAct \HNil -> [||()||] -- TODO: popLayoutContext
    ]

rLayoutSemicolon :: RuleExpr ()
rLayoutSemicolon = ruleExpr
    [ alt $ tokA ";"
        <:> semAct \(_ :* HNil) -> [||()||]
    , alt $ tokA @"vsemi"
        <:> semAct \(_ :* HNil) -> [||()||]
    ]
