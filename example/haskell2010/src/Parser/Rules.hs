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
import           Language.Parser.Ptera.Data.HEnum  (henum)
import           Language.Parser.Ptera.Data.HList  (HList (..))
import qualified Language.Parser.Ptera.Data.Record as Record
import           Language.Parser.Ptera.TH          hiding (RuleExpr, Rules)
import qualified Language.Parser.Ptera.TH          as Ptera
import           Language.Parser.Ptera.Util
import           Types
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Sequence as Seq
import qualified Language.Haskell.TH as TH
import           Data.Coerce
import           Data.Foldable
import qualified Numeric


$(genGrammarToken (TH.mkName "Tokens") [t|Token|]
    [ ("(",         [p|TokSpOpenParen{}|])
    , (")",         [p|TokSpCloseParen{}|])
    , (",",         [p|TokSpComma{}|])
    , (";",         [p|TokSpSemicolon{}|])
    , ("[",         [p|TokSpOpenBracket{}|])
    , ("]",         [p|TokSpCloseBracket{}|])
    , ("`",         [p|TokSpBacktick{}|])
    , ("{",         [p|TokSpOpenBrace{}|])
    , ("}",         [p|TokSpCloseBrace{}|])

    , ("{n}",       [p|TokVirtExpBrace{}|])
    , ("<n>",       [p|TokVirtNewline{}|])

    , ("case",      [p|TokKwCase{}|])
    , ("class",     [p|TokKwClass{}|])
    , ("data",      [p|TokKwData{}|])
    , ("default",   [p|TokKwDefault{}|])
    , ("deriving",  [p|TokKwDeriving{}|])
    , ("do",        [p|TokKwDo{}|])
    , ("else",      [p|TokKwElse{}|])
    , ("foreign",   [p|TokKwForeign{}|])
    , ("if",        [p|TokKwIf{}|])
    , ("import",    [p|TokKwImport{}|])
    , ("in",        [p|TokKwIn{}|])
    , ("infix",     [p|TokKwInfix{}|])
    , ("infixl",    [p|TokKwInfixl{}|])
    , ("infixr",    [p|TokKwInfixr{}|])
    , ("instance",  [p|TokKwInstance{}|])
    , ("let",       [p|TokKwLet{}|])
    , ("module",    [p|TokKwModule{}|])
    , ("newtype",   [p|TokKwNewtype{}|])
    , ("of",        [p|TokKwOf{}|])
    , ("then",      [p|TokKwThen{}|])
    , ("type",      [p|TokKwType{}|])
    , ("where",     [p|TokKwWhere{}|])
    , ("_",         [p|TokKwUnderscore{}|])

    , ("..",        [p|TokSymDots{}|])
    , (":",         [p|TokSymColon{}|])
    , ("::",        [p|TokSymDoubleColon{}|])
    , ("=",         [p|TokSymEqual{}|])
    , ("\\",        [p|TokSymBackslash{}|])
    , ("|",         [p|TokSymBar{}|])
    , ("<-",        [p|TokSymLeftArrow{}|])
    , ("->",        [p|TokSymRightArrow{}|])
    , ("@",         [p|TokSymAt{}|])
    , ("-",         [p|TokSymMinus{}|])
    , ("~",         [p|TokSymTilde{}|])
    , ("=>",        [p|TokSymRightDoubleArrow{}|])

    , ("qvarid",    [p|TokQualifiedVarId{}|])
    , ("qconid",    [p|TokQualifiedConId{}|])
    , ("qvarsym",   [p|TokQualifiedVarSym{}|])
    , ("qconsym",   [p|TokQualifiedConSym{}|])

    , ("integer",   [p|TokLitInteger{}|])
    , ("float",     [p|TokLitFloat{}|])
    , ("char",      [p|TokLitChar{}|])
    , ("string",    [p|TokLitString{}|])
    ])

grammar :: GrammarM GrammarContext ParsePoints Rules Tokens Token
grammar = fixGrammar $ Record.fromFieldsA $
    Record.field #module rModule :*
    Record.field #body rBody :*
    Record.field #bodyinl rBodyInL :*

    Record.field #impdecls rImpDecls :*
    Record.field #exports rExports :*
    Record.field #exports1 rExports1 :*
    Record.field #export rExport :*
    Record.field #cnames rCnames :*
    Record.field #cnames1 rCnames1 :*
    Record.field #impdecl rImpDecl :*
    Record.field #asmodidopt rAsModIdOpt :*
    Record.field #impspecopt rImpSpecOpt :*
    Record.field #impspec rImpSpec :*
    Record.field #imports rImports :*
    Record.field #import rImport :*
    Record.field #cname rCname :*

    Record.field #topdecls rTopDecls :*
    Record.field #topdecls1 rTopDecls1 :*
    Record.field #topdecl rTopDecl :*
    Record.field #derivingopt rDerivingOpt :*
    Record.field #contextopt rContextOpt :*
    Record.field #scontextopt rScontextOpt :*
    Record.field #wherecdeclsopt rWhereCdeclsOpt :*
    Record.field #whereideclsopt rWhereIdeclsOpt :*
    Record.field #types rTypes :*
    Record.field #types1 rTypes1 :*

    Record.field #decls rDecls :*
    Record.field #declsinl rDeclsInL :*
    Record.field #declsinl1 rDeclsInL1 :*
    Record.field #decl rDecl :*
    Record.field #cdecls rCdecls :*
    Record.field #cdeclsinl rCdeclsInL :*
    Record.field #cdeclsinl1 rCdeclsInL1 :*
    Record.field #cdecl rCdecl :*
    Record.field #idecls rIdecls :*
    Record.field #ideclsinl rIdeclsInL :*
    Record.field #ideclsinl1 rIdeclsInL1 :*
    Record.field #idecl rIdecl :*
    Record.field #gendecl rGenDecl :*
    Record.field #ops rOps :*
    Record.field #vars rVars :*
    Record.field #fixity rFixity :*

    Record.field #type rType :*
    Record.field #btype rBtype :*
    Record.field #atypes rAtypes :*
    Record.field #atype rAtype :*
    Record.field #types2 rTypes2 :*
    Record.field #gtycon rGtycon :*
    Record.field #commas1 rCommas1 :*

    Record.field #context rContext :*
    Record.field #classes rClasses :*
    Record.field #classes1 rClasses1 :*
    Record.field #class rClass :*
    Record.field #atypes1 rAtypes1 :*
    Record.field #scontext rScontext :*
    Record.field #simpleclasses rSimpleClasses :*
    Record.field #simpleclasses1 rSimpleClasses1 :*
    Record.field #simpleclass rSimpleClass :*

    Record.field #simpletype rSimpleType :*
    Record.field #tyvars rTyVars :*
    Record.field #constrs rConstrs :*
    Record.field #constr rConstr :*
    Record.field #fielddecls rFieldDecls :*
    Record.field #fielddecls1 rFieldDecls1 :*
    Record.field #abctype rAbctype :*
    Record.field #actypes rActypes :*
    Record.field #actype rActype :*
    Record.field #newconstr rNewConstr :*
    Record.field #fielddecl rFieldDecl :*
    Record.field #deriving rDeriving :*
    Record.field #dclasses rDclasses :*
    Record.field #dclasses1 rDclasses1 :*
    Record.field #dclass rDclass :*
    Record.field #inst rInst :*
    Record.field #tyvars2 rTyVars2 :*

    Record.field #fdecl rFdecl :*
    Record.field #callconv rCallConv :*
    Record.field #safetyopt rSafetyOpt :*
    Record.field #impent rImpent :*
    Record.field #expent rExpent :*
    Record.field #safety rSafety :*
    Record.field #ftype rFtype :*
    Record.field #frtype rFrtype :*
    Record.field #fatype rFatype :*

    Record.field #funlhs rFunlhs :*
    Record.field #apats1 rApats1 :*
    Record.field #rhs rRhs :*
    Record.field #wheredeclsopt rWhereDeclsOpt :*
    Record.field #gdrhs rGdrhs :*
    Record.field #guards rGuards :*
    Record.field #guards1 rGuards1 :*
    Record.field #guard rGuard :*

    Record.field #exp rExp :*
    Record.field #infixexp rInfixExp :*
    Record.field #lexp rLexp :*
    Record.field #semiopt rSemiopt :*
    Record.field #fexp rFexp :*
    Record.field #aexps rAexps :*
    Record.field #aexp rAexp :*
    Record.field #exps2 rExps2 :*
    Record.field #exps1 rExps1 :*
    Record.field #cexpopt rCexpOpt :*
    Record.field #expopt rExpOpt :*
    Record.field #quals1 rQuals1 :*
    Record.field #fbinds rFbinds :*
    Record.field #fbinds1 rFbinds1 :*

    Record.field #qual rQual :*
    Record.field #casealts rCaseAlts :*
    Record.field #alts rAlts :*
    Record.field #alt rAlt :*
    Record.field #gdpat rGdpat :*
    Record.field #dostmts rDoStmts :*
    Record.field #stmts rStmts :*
    Record.field #stmts0 rStmts0 :*
    Record.field #stmt rStmt :*
    Record.field #fbind rFbind :*

    Record.field #pat rPat :*
    Record.field #lpat rLpat :*
    Record.field #apat rApat :*
    Record.field #pats2 rPats2 :*
    Record.field #pats1 rPats1 :*
    Record.field #fpats rFpats :*
    Record.field #fpats1 rFpats1 :*
    Record.field #fpat rFpat :*

    Record.field #gcon rGcon :*
    Record.field #var rVar :*
    Record.field #qvar rQvar :*
    Record.field #con rCon :*
    Record.field #qcon rQcon :*
    Record.field #varop rVarOp :*
    Record.field #qvarop rQvarOp :*
    Record.field #conop rConOp :*
    Record.field #qconop rQconOp :*
    Record.field #op rOp :*
    Record.field #qop rQop :*
    Record.field #gconsym rGconsym :*
    Record.field #tyvar rTyvar :*
    Record.field #tycon rTycon :*
    Record.field #qtycon rQtycon :*
    Record.field #varid rVarId :*
    Record.field #varsym rVarSym :*
    Record.field #conid rConId :*
    Record.field #consym rConSym :*
    Record.field #qvarid rQvarId :*
    Record.field #qvarsym rQvarSym :*
    Record.field #qconid rQconId :*
    Record.field #qconsym rQconSym :*
    Record.field #modid rModId :*
    Record.field #exportid rExportId :*
    Record.field #hiding rHiding :*
    Record.field #as rAs :*
    Record.field #qualified rQualified :*
    Record.field (Record.FieldLabel :: Record.FieldLabel "!") rExclamationSym :*

    Record.field #literal rLiteral :*
    Record.field #integer rInteger :*
    Record.field #float rFloat :*
    Record.field #string rString :*
    Record.field #char rChar :*

    Record.field (Record.FieldLabel :: Record.FieldLabel "{") rExpBo :*
    Record.field (Record.FieldLabel :: Record.FieldLabel "}") rExpBc :*
    Record.field #impbo rImpBo :*
    Record.field #impbc rImpBc :*
    Record.field (Record.FieldLabel :: Record.FieldLabel ";") rSemi :*

    HNil

type ParsePoints = '[ "module" ]
type Rules =
    '[
        '("module", Program),
        '("body", ProgramBody),
        '("bodyinl", ProgramBody),

        '("impdecls", Seq.Seq ImportDecl),
        '("exports", [ExportItem]),
        '("exports1", Seq.Seq ExportItem),
        '("export", ExportItem),
        '("cnames", [Id]),
        '("cnames1", Seq.Seq Id),
        '("impdecl", ImportDecl),
        '("asmodidopt", Maybe QualifiedId),
        '("impspecopt", Maybe ImportSpec),
        '("impspec", ImportSpec),
        '("imports", Seq.Seq ImportItem),
        '("import", ImportItem),
        '("cname", Id),

        '("topdecls", [Decl]),
        '("topdecls1", Seq.Seq Decl),
        '("topdecl", Seq.Seq Decl),
        '("derivingopt", Maybe Deriving),
        '("contextopt", Maybe Context),
        '("scontextopt", Maybe Context),
        '("wherecdeclsopt", [Decl]),
        '("whereideclsopt", [Decl]),
        '("types", [Type]),
        '("types1", Seq.Seq Type),

        '("decls", [Decl]),
        '("declsinl", [Decl]),
        '("declsinl1", Seq.Seq Decl),
        '("decl", Seq.Seq Decl),
        '("cdecls", [Decl]),
        '("cdeclsinl", [Decl]),
        '("cdeclsinl1", Seq.Seq Decl),
        '("cdecl", Seq.Seq Decl),
        '("idecls", [Decl]),
        '("ideclsinl", [Decl]),
        '("ideclsinl1", Seq.Seq Decl),
        '("idecl", Seq.Seq Decl),
        '("gendecl", Seq.Seq Decl),
        '("ops", Seq.Seq Id),
        '("vars", Seq.Seq Id),
        '("fixity", Fixity),

        '("type", Type),
        '("btype", Type),
        '("atypes", Seq.Seq Type),
        '("atype", Type),
        '("types2", Seq.Seq Type),
        '("gtycon", Type),
        '("commas1", Int),

        '("context", Context),
        '("classes", [Type]),
        '("classes1", Seq.Seq Type),
        '("class", Type),
        '("atypes1", Seq.Seq Type),
        '("scontext", Context),
        '("simpleclasses", [Type]),
        '("simpleclasses1", Seq.Seq Type),
        '("simpleclass", Type),

        '("simpletype", Type),
        '("tyvars", Seq.Seq Type),
        '("constrs", Seq.Seq Constr),
        '("constr", Constr),
        '("fielddecls", [(Strictness, [Id], Type)]),
        '("fielddecls1", Seq.Seq (Strictness, [Id], Type)),
        '("abctype", (Strictness, Type)),
        '("actypes", Seq.Seq (Strictness, Type)),
        '("actype", (Strictness, Type)),
        '("newconstr", Constr),
        '("fielddecl", (Strictness, [Id], Type)),
        '("deriving", Deriving),
        '("dclasses", [Type]),
        '("dclasses1", Seq.Seq Type),
        '("dclass", Type),
        '("inst", Type),
        '("tyvars2", Seq.Seq Type),

        '("fdecl", Decl),
        '("callconv", ForeignCallConv),
        '("safetyopt", Maybe Safety),
        '("impent", Maybe String),
        '("expent", Maybe String),
        '("safety", Safety),
        '("ftype", Type),
        '("frtype", Type),
        '("fatype", Type),

        '("funlhs", (Id, Seq.Seq Pat)),
        '("apats1", Seq.Seq Pat),
        '("rhs", Rhs),
        '("wheredeclsopt", [Decl]),
        '("gdrhs", Seq.Seq ([Guard], Exp)),
        '("guards", [Guard]),
        '("guards1", Seq.Seq Guard),
        '("guard", Guard),

        '("exp", Exp),
        '("infixexp", Exp),
        '("lexp", Exp),
        '("semiopt", ()),
        '("fexp", Exp),
        '("aexps", Seq.Seq Exp),
        '("aexp", Exp),
        '("exps2", Seq.Seq Exp),
        '("exps1", Seq.Seq Exp),
        '("cexpopt", Maybe Exp),
        '("expopt", Maybe Exp),
        '("quals1", Seq.Seq Guard),
        '("fbinds", [(QualifiedId, Exp)]),
        '("fbinds1", Seq.Seq (QualifiedId, Exp)),

        '("qual", Guard),
        '("casealts", [CaseAlt]),
        '("alts", Seq.Seq CaseAlt),
        '("alt", Seq.Seq CaseAlt),
        '("gdpat", Seq.Seq ([Guard], Exp)),
        '("dostmts", ([Stmt], Exp)),
        '("stmts", ([Stmt], Exp)),
        '("stmts0", Seq.Seq Stmt),
        '("stmt", Seq.Seq Stmt),
        '("fbind", (QualifiedId, Exp)),

        '("pat", Pat),
        '("lpat", Pat),
        '("apat", Pat),
        '("pats2", Seq.Seq Pat),
        '("pats1", Seq.Seq Pat),
        '("fpats", [(QualifiedId, Pat)]),
        '("fpats1", Seq.Seq (QualifiedId, Pat)),
        '("fpat", (QualifiedId, Pat)),

        '("gcon", Gcon),
        '("var", Id),
        '("qvar", QualifiedId),
        '("con", Id),
        '("qcon", QualifiedId),
        '("varop", Id),
        '("qvarop", QualifiedId),
        '("conop", Id),
        '("qconop", QualifiedId),
        '("op", Id),
        '("qop", QualifiedId),
        '("gconsym", QualifiedId),
        '("tyvar", Id),
        '("tycon", Id),
        '("qtycon", QualifiedId),
        '("varid", Id),
        '("varsym", Id),
        '("conid", Id),
        '("consym", Id),
        '("qvarid", QualifiedId),
        '("qvarsym", QualifiedId),
        '("qconid", QualifiedId),
        '("qconsym", QualifiedId),
        '("modid", QualifiedId),
        '("exportid", ()),
        '("hiding", ()),
        '("as", ()),
        '("qualified", ()),
        '("!", ()),

        '("literal", Lit),
        '("integer", Integer),
        '("float", Rational),
        '("string", String),
        '("char", Char),

        '("{", ()),
        '("}", ()),
        '("impbo", ()),
        '("impbc", ()),
        '(";", ())
    ]

type RuleExpr = Ptera.RuleExprM GrammarContext Rules Tokens Token
type Unit = Ptera.Unit Rules Tokens Token

type GrammarContext = [Int]

rModule :: RuleExpr Program
rModule = ruleExpr
    [ alt $ tokA @"module" <^> varA @"modid" <^> varA @"exports" <^> tokA @"where" <^> varA @"body"
        <:> semAct \(_ :* modId :* exports :* _ :* body :* HNil) ->
            [||Program (Just $$(modId)) $$(exports) $$(body)||]
    , alt $ tokA @"module" <^> varA @"modid" <^> tokA @"where" <^> varA @"body"
        <:> semAct \(_ :* modId :* _ :* body :* HNil) ->
            [||Program (Just $$(modId)) [] $$(body)||]
    , alt $ varA @"body"
        <:> semAct \(body :* HNil) ->
            [||Program Nothing [] $$(body)||]
    ]

rBody :: RuleExpr ProgramBody
rBody = ruleExpr
    [ alt $ varA @"{" <^> varA @"bodyinl" <^> varA @"}"
        <:> semAct \(_ :* body :* _ :* HNil) -> body
    , alt $ varA @"impbo" <^> varA @"bodyinl" <^> varA @"impbc"
        <:> semAct \(_ :* body :* _ :* HNil) -> body
    ]

rBodyInL :: RuleExpr ProgramBody
rBodyInL = ruleExpr
    [ alt $ varA @"impdecls" <^> varA @";" <^> varA @"topdecls"
        <:> semAct \(impdecls :* _ :* topdecls :* HNil) ->
            [||ProgramBody (toList $$(impdecls)) $$(topdecls)||]
    , alt $ varA @"impdecls"
        <:> semAct \(impdecls :* HNil) ->
            [||ProgramBody (toList $$(impdecls)) []||]
    , alt $ varA @"topdecls"
        <:> semAct \(topdecls :* HNil) ->
            [||ProgramBody [] $$(topdecls)||]
    ]

rImpDecls :: RuleExpr (Seq.Seq ImportDecl)
rImpDecls = ruleExpr
    [ alt $ varA @"impdecl" <^> varA @";" <^> varA @"impdecls"
        <:> semAct \(impdecl :* _ :* impdecls :* HNil) ->
            [||$$(impdecl) Seq.:<| $$(impdecls)||]
    , alt $ varA @"impdecl"
        <:> semAct \(impdecl :* HNil) ->
            [||pure $$(impdecl)||]
    ]

rExports :: RuleExpr [ExportItem]
rExports = ruleExpr
    [ alt $ tokA @"(" <^> varA @"exports1" <^> tokA @")"
        <:> semAct \(_ :* exports :* _ :* HNil) ->
            [||toList $$(exports)||]
    ]

rExports1 :: RuleExpr (Seq.Seq ExportItem)
rExports1 = ruleExpr
    [ alt $ varA @"export" <^> tokA @"," <^> varA @"exports1"
        <:> semAct \(export :* _ :* exports :* HNil) ->
            [||$$(export) Seq.:<| $$(exports)||]
    , alt $ varA @"export"
        <:> semAct \(export :* HNil) ->
            [||pure $$(export)||]
    , eps
        $ semAct \HNil ->
            [||Seq.empty||]
    ]

rExport :: RuleExpr ExportItem
rExport = ruleExpr
    [ alt $ varA @"qvar"
        <:> semAct \(qvar :* HNil) ->
            [||ExportItemId $$(qvar)||]
    , alt $ varA @"qtycon" <^> tokA @"(" <^> tokA @".." <^> tokA @")"
        <:> semAct \(qtycon :* _ :* _ :* _ :* HNil) ->
            [||ExportItemTyConAll $$(qtycon)||]
    , alt $ varA @"qtycon" <^> tokA @"(" <^> varA @"cnames" <^> tokA @")"
        <:> semAct \(qtycon :* _ :* cnames :* _ :* HNil) ->
            [||ExportItemTyConSpecified $$(qtycon) (toList $$(cnames))||]
    ]

rCnames :: RuleExpr [Id]
rCnames = ruleExpr
    [ alt $ varA @"cnames1"
        <:> semAct \(cnames1 :* HNil) ->
            [||toList $$(cnames1)||]
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rCnames1 :: RuleExpr (Seq.Seq Id)
rCnames1 = ruleExpr
    [ alt $ varA @"cname" <^> tokA @"," <^> varA @"cnames1"
        <:> semAct \(cname :* _ :* cnames1 :* HNil) ->
            [||$$(cname) Seq.:<| $$(cnames1)||]
    , alt $ varA @"cname"
        <:> semAct \(cname :* HNil) ->
            [||pure $$(cname)||]
    ]

rImpDecl :: RuleExpr ImportDecl
rImpDecl = ruleExpr
    [ alt $ tokA @"import" <^> varA @"qualified" <^> varA @"modid" <^> varA @"asmodidopt" <^> varA @"impspecopt"
        <:> semAct \(_ :* _ :* modid :* asmodidOpt :* impspecOpt :* HNil) ->
            [||ImportDecl True $$(modid) $$(asmodidOpt) $$(impspecOpt)||]
    , alt $ tokA @"import" <^> varA @"modid" <^> varA @"asmodidopt" <^> varA @"impspecopt"
        <:> semAct \(_ :* modid :* asmodidOpt :* impspecOpt :* HNil) ->
            [||ImportDecl False $$(modid) $$(asmodidOpt) $$(impspecOpt)||]
    ]

rAsModIdOpt :: RuleExpr (Maybe QualifiedId)
rAsModIdOpt = ruleExpr
    [ alt $ varA @"as" <^> varA @"modid"
        <:> semAct \(_ :* modid :* HNil) ->
            [||Just $$(modid)||]
    , eps
        $ semAct \HNil ->
            [||Nothing||]
    ]

rImpSpecOpt :: RuleExpr (Maybe ImportSpec)
rImpSpecOpt = ruleExpr
    [ alt $ varA @"impspec"
        <:> semAct \(impspec :* HNil) ->
            [||Just $$(impspec)||]
    , eps
        $ semAct \HNil ->
            [||Nothing||]
    ]

rImpSpec :: RuleExpr ImportSpec
rImpSpec = ruleExpr
    [ alt $ tokA @"(" <^> varA @"imports" <^> tokA @")"
        <:> semAct \(_ :* imports :* _ :* HNil) ->
            [||ImportSpecSpecified (toList $$(imports))||]
    , alt $ varA @"hiding" <^> tokA @"(" <^> varA @"imports" <^> tokA @")"
        <:> semAct \(_ :* _ :* imports :* _ :* HNil) ->
            [||ImportSpecHiding (toList $$(imports))||]
    ]

rImports :: RuleExpr (Seq.Seq ImportItem)
rImports = ruleExpr
    [ alt $ varA @"import" <^> tokA @"," <^> varA @"imports"
        <:> semAct \(imp :* _ :* imports :* HNil) ->
            [||$$(imp) Seq.:<| $$(imports)||]
    , alt $ varA @"import"
        <:> semAct \(imp :* HNil) ->
            [||pure $$(imp)||]
    , eps
        $ semAct \HNil ->
            [||Seq.empty||]
    ]

rImport :: RuleExpr ImportItem
rImport = ruleExpr
    [ alt $ varA @"var"
        <:> semAct \(var :* HNil) ->
            [||ImportItemId $$(var)||]
    , alt $ varA @"tycon" <^> tokA @"(" <^> tokA @".." <^> tokA @")"
        <:> semAct \(tycon :* _ :* _ :* _ :* HNil) ->
            [||ImportItemTyConAll $$(tycon)||]
    , alt $ varA @"tycon" <^> tokA @"(" <^> varA @"cnames" <^> tokA @")"
        <:> semAct \(tycon :* _ :* cnames :* _ :* HNil) ->
            [||ImportItemTyConSpecified $$(tycon) $$(cnames)||]
    ]

rCname :: RuleExpr Id
rCname = ruleExpr
    [ alt $ varA @"var"
        <:> semAct \(var :* HNil) ->
            var
    , alt $ varA @"con"
        <:> semAct \(con :* HNil) ->
            con
    ]

rTopDecls :: RuleExpr [Decl]
rTopDecls = ruleExpr
    [ alt $ varA @"topdecls1"
        <:> semAct \(topdecls1 :* HNil) ->
            [||toList $$(topdecls1)||]
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rTopDecls1 :: RuleExpr (Seq.Seq Decl)
rTopDecls1 = ruleExpr
    [ alt $ varA @"topdecl" <^> tokA @":" <^> varA @"topdecls1"
        <:> semAct \(topdecl :* _ :* topdecls1 :* HNil) ->
            [||$$(topdecl) <> $$(topdecls1)||]
    , alt $ varA @"topdecl"
        <:> semAct \(topdecl :* HNil) ->
            topdecl
    ]

rTopDecl :: RuleExpr (Seq.Seq Decl)
rTopDecl = ruleExpr
    [ alt $ tokA @"type" <^> varA @"simpletype" <^> tokA @"=" <^> varA @"type"
        <:> semAct \(_ :* simpletype :* _ :* ty :* HNil) ->
            [||pure $ DeclType $$(simpletype) $$(ty)||]
    , alt $ tokA @"data" <^> varA @"contextopt" <^> varA @"simpletype" <^> tokA @"=" <^> varA @"constrs" <^> varA @"derivingopt"
        <:> semAct \(_ :* contextopt :* simpletype :* _ :* constrs :* derivingopt :* HNil) ->
            [||pure $ DeclData $$(contextopt) $$(simpletype) (toList $$(constrs)) $$(derivingopt)||]
    , alt $ tokA @"data" <^> varA @"contextopt" <^> varA @"simpletype" <^> varA @"derivingopt"
        <:> semAct \(_ :* contextopt :* simpletype :* derivingopt :* HNil) ->
            [||pure $ DeclData $$(contextopt) $$(simpletype) [] $$(derivingopt)||]
    , alt $ tokA @"newtype" <^> varA @"contextopt" <^> varA @"simpletype" <^> tokA @"=" <^> varA @"newconstr" <^> varA @"derivingopt"
        <:> semAct \(_ :* contextopt :* simpletype :* _ :* newconstr :* derivingopt :* HNil) ->
            [||pure $ DeclNewtype $$(contextopt) $$(simpletype) $$(newconstr) $$(derivingopt)||]
    , alt $ tokA @"class" <^> varA @"scontextopt" <^> varA @"tycon" <^> varA @"tyvar" <^> varA @"wherecdeclsopt"
        <:> semAct \(_ :* contextopt :* tycon :* tyvar :* cdecls :* HNil) ->
            [||pure $ DeclClass $$(contextopt) $$(tycon) $$(tyvar) $$(cdecls)||]
    , alt $ tokA @"instance" <^> varA @"scontextopt" <^> varA @"qtycon" <^> varA @"inst" <^> varA @"whereideclsopt"
        <:> semAct \(_ :* contextopt :* qtycon :* inst :* idecls :* HNil) ->
            [||pure $ DeclInstance $$(contextopt) $$(qtycon) $$(inst) $$(idecls)||]
    , alt $ tokA @"default" <^> tokA @"(" <^> varA @"types" <^> tokA @")"
        <:> semAct \(_ :* _ :* types :* _ :* HNil) ->
            [||pure $ DeclDefault $$(types)||]
    , alt $ tokA @"foreign" <^> varA @"fdecl"
        <:> semAct \(_ :* fdecl :* HNil) ->
            [||pure $$(fdecl)||]
    , alt $ varA @"decl"
        <:> semAct \(decl :* HNil) ->
            decl
    ]

rDerivingOpt :: RuleExpr (Maybe Deriving)
rDerivingOpt = ruleExpr
    [ alt $ varA @"deriving"
        <:> semAct \(deriv :* HNil) ->
            [||Just $$(deriv)||]
    , eps
        $ semAct \HNil ->
            [||Nothing||]
    ]

rContextOpt :: RuleExpr (Maybe Context)
rContextOpt = ruleExpr
    [ alt $ varA @"context" <^> tokA @"=>"
        <:> semAct \(context :* _ :* HNil) ->
            [||Just $$(context)||]
    , eps
        $ semAct \HNil ->
            [||Nothing||]
    ]

rScontextOpt :: RuleExpr (Maybe Context)
rScontextOpt = ruleExpr
    [ alt $ varA @"scontext" <^> tokA @"=>"
        <:> semAct \(context :* _ :* HNil) ->
            [||Just $$(context)||]
    , eps
        $ semAct \HNil ->
            [||Nothing||]
    ]

rWhereCdeclsOpt :: RuleExpr [Decl]
rWhereCdeclsOpt = ruleExpr
    [ alt $ tokA @"where" <^> varA @"cdecls"
        <:> semAct \(_ :* cdecls :* HNil) ->
            cdecls
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rWhereIdeclsOpt :: RuleExpr [Decl]
rWhereIdeclsOpt = ruleExpr
    [ alt $ tokA @"where" <^> varA @"idecls"
        <:> semAct \(_ :* idecls :* HNil) ->
            idecls
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rTypes :: RuleExpr [Type]
rTypes = ruleExpr
    [ alt $ varA @"types1"
        <:> semAct \(types1 :* HNil) ->
            [||toList $$(types1)||]
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rTypes1 :: RuleExpr (Seq.Seq Type)
rTypes1 = ruleExpr
    [ alt $ varA @"type" <^> tokA @"," <^> varA @"types1"
        <:> semAct \(ty :* _ :* types1 :* HNil) ->
            [||$$(ty) Seq.:<| $$(types1)||]
    , alt $ varA @"type"
        <:> semAct \(ty :* HNil) ->
            [||pure $$(ty)||]
    ]

rDecls :: RuleExpr [Decl]
rDecls = ruleExpr
    [ alt $ varA @"{" <^> varA @"declsinl" <^> varA @"}"
        <:> semAct \(_ :* declsinl :* _ :* HNil) ->
            declsinl
    , alt $ varA @"impbo" <^> varA @"declsinl" <^> varA @"impbc"
        <:> semAct \(_ :* declsinl :* _ :* HNil) ->
            declsinl
    ]

rDeclsInL :: RuleExpr [Decl]
rDeclsInL = ruleExpr
    [ alt $ varA @"declsinl1"
        <:> semAct \(declsinl1 :* HNil) ->
            [||toList $$(declsinl1)||]
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rDeclsInL1 :: RuleExpr (Seq.Seq Decl)
rDeclsInL1 = ruleExpr
    [ alt $ varA @"decl" <^> varA @";" <^> varA @"declsinl1"
        <:> semAct \(decl :* _ :* declsinl1 :* HNil) ->
            [||$$(decl) <> $$(declsinl1)||]
    , alt $ varA @"decl"
        <:> semAct \(decl :* HNil) ->
            decl
    ]

rDecl :: RuleExpr (Seq.Seq Decl)
rDecl = ruleExpr
    [ alt $ varA @"gendecl"
        <:> semAct \(gendecl :* HNil) ->
            gendecl
    , alt $ varA @"funlhs" <^> varA @"rhs"
        <:> semAct \(funlhs :* rhs :* HNil) ->
            [||case $$(funlhs) of
                (f, args) ->
                    pure $ DeclFun f (toList args) $$(rhs)
            ||]
    , alt $ varA @"pat" <^> varA @"rhs"
        <:> semAct \(pat :* rhs :* HNil) ->
            [||pure $ DeclVar $$(pat) $$(rhs)||]
    ]

rCdecls :: RuleExpr [Decl]
rCdecls = ruleExpr
    [ alt $ varA @"{" <^> varA @"cdeclsinl" <^> varA @"}"
        <:> semAct \(_ :* cdeclsinl :* _ :* HNil) ->
            cdeclsinl
    , alt $ varA @"impbo" <^> varA @"cdeclsinl" <^> varA @"impbc"
        <:> semAct \(_ :* cdeclsinl :* _ :* HNil) ->
            cdeclsinl
    ]

rCdeclsInL :: RuleExpr [Decl]
rCdeclsInL = ruleExpr
    [ alt $ varA @"cdeclsinl1"
        <:> semAct \(cdeclsinl1 :* HNil) ->
            [||toList $$(cdeclsinl1)||]
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rCdeclsInL1 :: RuleExpr (Seq.Seq Decl)
rCdeclsInL1 = ruleExpr
    [ alt $ varA @"cdecl" <^> varA @";" <^> varA @"cdeclsinl1"
        <:> semAct \(cdecl :* _ :* cdeclsinl1 :* HNil) ->
            [||$$(cdecl) <> $$(cdeclsinl1)||]
    , alt $ varA @"cdecl"
        <:> semAct \(cdecl :* HNil) ->
            cdecl
    ]

rCdecl :: RuleExpr (Seq.Seq Decl)
rCdecl = ruleExpr
    [ alt $ varA @"gendecl"
        <:> semAct \(gendecl :* HNil) ->
            gendecl
    , alt $ varA @"funlhs" <^> varA @"rhs"
        <:> semAct \(funlhs :* rhs :* HNil) ->
            [||case $$(funlhs) of
                (f, args) ->
                    pure $ DeclFun f (toList args) $$(rhs)
            ||]
    , alt $ varA @"var" <^> varA @"rhs"
        <:> semAct \(var :* rhs :* HNil) ->
            [||pure $ DeclVar (PatId $$(var) Nothing) $$(rhs)||]
    ]

rIdecls :: RuleExpr [Decl]
rIdecls = ruleExpr
    [ alt $ varA @"{" <^> varA @"ideclsinl" <^> varA @"}"
        <:> semAct \(_ :* ideclsinl :* _ :* HNil) ->
            ideclsinl
    , alt $ varA @"impbo" <^> varA @"ideclsinl" <^> varA @"impbc"
        <:> semAct \(_ :* ideclsinl :* _ :* HNil) ->
            ideclsinl
    ]

rIdeclsInL :: RuleExpr [Decl]
rIdeclsInL = ruleExpr
    [ alt $ varA @"ideclsinl1"
        <:> semAct \(ideclsinl1 :* HNil) ->
            [||toList $$(ideclsinl1)||]
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rIdeclsInL1 :: RuleExpr (Seq.Seq Decl)
rIdeclsInL1 = ruleExpr
    [ alt $ varA @"idecl" <^> varA @";" <^> varA @"ideclsinl1"
        <:> semAct \(idecl :* _ :* ideclsinl1 :* HNil) ->
            [||$$(idecl) <> $$(ideclsinl1)||]
    , alt $ varA @"idecl"
        <:> semAct \(idecl :* HNil) ->
            idecl
    ]

rIdecl :: RuleExpr (Seq.Seq Decl)
rIdecl = ruleExpr
    [ alt $ varA @"funlhs" <^> varA @"rhs"
        <:> semAct \(funlhs :* rhs :* HNil) ->
            [||case $$(funlhs) of
                (f, args) ->
                    pure $ DeclFun f (toList args) $$(rhs)
            ||]
    , alt $ varA @"var" <^> varA @"rhs"
        <:> semAct \(var :* rhs :* HNil) ->
            [||pure $ DeclVar (PatId $$(var) Nothing) $$(rhs)||]
    , eps
        $ semAct \HNil ->
            [||Seq.empty||]
    ]

rGenDecl :: RuleExpr (Seq.Seq Decl)
rGenDecl = ruleExpr
    [ alt $ varA @"vars" <^> tokA @"::" <^> varA @"contextopt" <^> varA @"type"
        <:> semAct \(vars :* _ :* contextopt :* ty :* HNil) ->
            [||pure $ DeclSig (toList $$(vars)) $$(contextopt) $$(ty)||]
    , alt $ varA @"fixity" <^> varA @"integer" <^> varA @"ops"
        <:> semAct \(fixity :* integer :* ops :* HNil) ->
            [||pure $ DeclFixity $$(fixity) (Just (fromInteger $$(integer))) (toList $$(ops))||]
    , alt $ varA @"fixity" <^> varA @"ops"
        <:> semAct \(fixity :* ops :* HNil) ->
            [||pure $ DeclFixity $$(fixity) Nothing (toList $$(ops))||]
    , eps
        $ semAct \HNil ->
            [||Seq.empty||]
    ]

rOps :: RuleExpr (Seq.Seq Id)
rOps = ruleExpr
    [ alt $ varA @"op" <^> tokA @"," <^> varA @"ops"
        <:> semAct \(op :* _ :* ops :* HNil) ->
            [||$$(op) Seq.:<| $$(ops)||]
    , alt $ varA @"op"
        <:> semAct \(op :* HNil) ->
            [||pure $$(op)||]
    ]

rVars :: RuleExpr (Seq.Seq Id)
rVars = ruleExpr
    [ alt $ varA @"var" <^> tokA @"," <^> varA @"vars"
        <:> semAct \(var :* _ :* vars :* HNil) ->
            [||$$(var) Seq.:<| $$(vars)||]
    , alt $ varA @"var"
        <:> semAct \(var :* HNil) ->
            [||pure $$(var)||]
    ]

rFixity :: RuleExpr Fixity
rFixity = ruleExpr
    [ alt $ tokA @"infixl"
        <:> semAct \(_ :* HNil) ->
            [||FixityInfixL||]
    , alt $ tokA @"infixr"
        <:> semAct \(_ :* HNil) ->
            [||FixityInfixR||]
    , alt $ tokA @"infix"
        <:> semAct \(_ :* HNil) ->
            [||FixityInfix||]
    ]

rType :: RuleExpr Type
rType = ruleExpr
    [ alt $ varA @"btype" <^> tokA @"->" <^> varA @"type"
        <:> semAct \(btype :* _ :* ty :* HNil) ->
            [||TypeArrow $$(btype) $$(ty)||]
    , alt $ varA @"btype"
        <:> semAct \(btype :* HNil) ->
            btype
    ]

rBtype :: RuleExpr Type
rBtype = ruleExpr
    [ alt $ varA @"atype" <^> varA @"atypes"
        <:> semAct \(atype :* atypes :* HNil) ->
            [||TypeApp $$(atype) (toList $$(atypes))||]
    ]

rAtypes :: RuleExpr (Seq.Seq Type)
rAtypes = ruleExpr
    [ alt $ varA @"atype" <^> varA @"atypes"
        <:> semAct \(atype :* atypes :* HNil) ->
            [||$$(atype) Seq.:<| $$(atypes)||]
    , eps
        $ semAct \HNil ->
            [||Seq.empty||]
    ]

rAtype :: RuleExpr Type
rAtype = ruleExpr
    [ alt $ varA @"gtycon"
        <:> semAct \(gtycon :* HNil) ->
            gtycon
    , alt $ varA @"tyvar"
        <:> semAct \(tyvar :* HNil) ->
            [||TypeId (nonQualifiedId $$(tyvar))||]
    , alt $ tokA @"(" <^> varA @"types2" <^> tokA @")"
        <:> semAct \(_ :* types2 :* _ :* HNil) ->
            [||TypeTuple (toList $$(types2))||]
    , alt $ tokA @"[" <^> varA @"type" <^> tokA @"]"
        <:> semAct \(_ :* ty :* _ :* HNil) ->
            [||TypeList $$(ty)||]
    , alt $ tokA @"(" <^> varA @"type" <^> tokA @")"
        <:> semAct \(_ :* ty :* _ :* HNil) ->
            ty
    ]

rTypes2 :: RuleExpr (Seq.Seq Type)
rTypes2 = ruleExpr
    [ alt $ varA @"type" <^> tokA @"," <^> varA @"types2"
        <:> semAct \(ty :* _ :* types2 :* HNil) ->
            [||$$(ty) Seq.:<| $$(types2)||]
    , alt $ varA @"type" <^> tokA @"," <^> varA @"type"
        <:> semAct \(ty1 :* _ :* ty2 :* HNil) ->
            [||Seq.fromList [$$(ty1), $$(ty2)]||]
    ]

rGtycon :: RuleExpr Type
rGtycon = ruleExpr
    [ alt $ varA @"qtycon"
        <:> semAct \(qtycon :* HNil) ->
            [||TypeId $$(qtycon)||]
    , alt $ tokA @"(" <^> tokA @")"
        <:> semAct \(_ :* _ :* HNil) ->
            [||TypeId $ nonQualifiedId $ mkId "()"||]
    , alt $ tokA @"[" <^> tokA @"]"
        <:> semAct \(_ :* _ :* HNil) ->
            [||TypeId $ nonQualifiedId $ mkId "[]"||]
    , alt $ tokA @"(" <^> tokA @"->" <^> tokA @")"
        <:> semAct \(_ :* _ :* _ :* HNil) ->
            [||TypeId $ nonQualifiedId $ mkId "->"||]
    , alt $ tokA @"(" <^> varA @"commas1" <^> tokA @")"
        <:> semAct \(_ :* i :* _ :* HNil) ->
            [||TypeTupleCon $$(i)||]
    ]

rCommas1 :: RuleExpr Int
rCommas1 = ruleExpr
    [ alt $ tokA @"," <^> varA @"commas1"
        <:> semAct \(_ :* i :* HNil) ->
            [||$$(i) + 1||]
    , alt $ tokA @","
        <:> semAct \(_ :* HNil) ->
            [||1||]
    ]

rContext :: RuleExpr Context
rContext = ruleExpr
    [ alt $ varA @"class"
        <:> semAct \(cls :* HNil) ->
            [||Context [$$(cls)]||]
    , alt $ tokA @"(" <^> varA @"classes" <^> tokA @")"
        <:> semAct \(_ :* classes :* _ :* HNil) ->
            [||Context $$(classes)||]
    ]

rClasses :: RuleExpr [Type]
rClasses = ruleExpr
    [ alt $ varA @"classes1"
        <:> semAct \(classes1 :* HNil) ->
            [||toList $$(classes1)||]
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rClasses1 :: RuleExpr (Seq.Seq Type)
rClasses1 = ruleExpr
    [ alt $ varA @"class" <^> tokA @"," <^> varA @"classes1"
        <:> semAct \(cls :* _ :* classes1 :* HNil) ->
            [||$$(cls) Seq.:<| $$(classes1)||]
    , alt $ varA @"class"
        <:> semAct \(cls :* HNil) ->
            [||pure $$(cls)||]
    ]

rClass :: RuleExpr Type
rClass = ruleExpr
    [ alt $ varA @"qtycon" <^> varA @"tyvar"
        <:> semAct \(qtycon :* tyvar :* HNil) ->
            [||TypeApp (TypeId $$(qtycon)) [TypeId (nonQualifiedId $$(tyvar))]||]
    , alt $ varA @"qtycon" <^> tokA @"(" <^> varA @"tyvar" <^> varA @"atypes1" <^> tokA @")"
        <:> semAct \(qtycon :* _ :* tyvar :* atypes1 :* _ :* HNil) ->
            [||TypeApp
                (TypeId $$(qtycon))
                [TypeApp (TypeId (nonQualifiedId $$(tyvar))) (toList $$(atypes1))]
            ||]
    ]

rAtypes1 :: RuleExpr (Seq.Seq Type)
rAtypes1 = ruleExpr
    [ alt $ varA @"atype" <^> varA @"atypes1"
        <:> semAct \(atype :* atypes1 :* HNil) ->
            [||$$(atype) Seq.:<| $$(atypes1)||]
    , alt $ varA @"atype"
        <:> semAct \(atype :* HNil) ->
            [||pure $$(atype)||]
    ]

rScontext :: RuleExpr Context
rScontext = ruleExpr
    [ alt $ varA @"simpleclass"
        <:> semAct \(simpleclass :* HNil) ->
            [||Context [$$(simpleclass)]||]
    , alt $ tokA @"(" <^> varA @"simpleclasses" <^> tokA @")"
        <:> semAct \(_ :* simpleclasses :* _ :* HNil) ->
            [||Context $$(simpleclasses)||]
    ]

rSimpleClasses :: RuleExpr [Type]
rSimpleClasses = ruleExpr
    [ alt $ varA @"simpleclasses1"
        <:> semAct \(simpleclasses1 :* HNil) ->
            [||toList $$(simpleclasses1)||]
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rSimpleClasses1 :: RuleExpr (Seq.Seq Type)
rSimpleClasses1 = ruleExpr
    [ alt $ varA @"simpleclass" <^> tokA @"," <^> varA @"simpleclasses1"
        <:> semAct \(simpleclass :* _ :* simpleclasses1 :* HNil) ->
            [||$$(simpleclass) Seq.:<| $$(simpleclasses1)||]
    , alt $ varA @"simpleclass"
        <:> semAct \(simpleclass :* HNil) ->
            [||pure $$(simpleclass)||]
    ]

rSimpleClass :: RuleExpr Type
rSimpleClass = ruleExpr
    [ alt $ varA @"qtycon" <^> varA @"tyvar"
        <:> semAct \(qtycon :* tyvar :* HNil) ->
            [||TypeApp (TypeId $$(qtycon)) [TypeId (nonQualifiedId $$(tyvar))]||]
    ]

rSimpleType :: RuleExpr Type
rSimpleType = ruleExpr
    [ alt $ varA @"tycon" <^> varA @"tyvars"
        <:> semAct \(tycon :* tyvars :* HNil) ->
            [||TypeApp (TypeId (nonQualifiedId $$(tycon))) (toList $$(tyvars))||]
    ]

rTyVars :: RuleExpr (Seq.Seq Type)
rTyVars = ruleExpr
    [ alt $ varA @"tyvar" <^> varA @"tyvars"
        <:> semAct \(tyvar :* tyvars :* HNil) ->
            [||TypeId (nonQualifiedId $$(tyvar)) Seq.:<| $$(tyvars)||]
    , eps
        $ semAct \HNil ->
            [||Seq.empty||]
    ]

rConstrs :: RuleExpr (Seq.Seq Constr)
rConstrs = ruleExpr
    [ alt $ varA @"constr" <^> tokA @"|" <^> varA @"constrs"
        <:> semAct \(constr :* _ :* constrs :* HNil) ->
            [||$$(constr) Seq.:<| $$(constrs)||]
    , alt $ varA @"constr"
        <:> semAct \(constr :* HNil) ->
            [||pure $$(constr)||]
    ]

rConstr :: RuleExpr Constr
rConstr = ruleExpr
    [ alt $ varA @"con" <^> tokA @"{" <^> varA @"fielddecls" <^> tokA @"}"
        <:> semAct \(con :* _ :* fielddecls :* _ :* HNil) ->
            [||ConstrWithFields $$(con) $$(fielddecls)||]
    , alt $ varA @"abctype" <^> varA @"conop" <^> varA @"abctype"
        <:> semAct \(abcty1 :* conop :* abcty2 :* HNil) ->
            [||ConstrApp $$(conop) [$$(abcty1), $$(abcty2)]||]
    , alt $ varA @"con" <^> varA @"actypes"
        <:> semAct \(con :* actypes :* HNil) ->
            [||ConstrApp $$(con) (toList $$(actypes))||]
    ]

rFieldDecls :: RuleExpr [(Strictness, [Id], Type)]
rFieldDecls = ruleExpr
    [ alt $ varA @"fielddecls1"
        <:> semAct \(fielddecls1 :* HNil) ->
            [||toList $$(fielddecls1)||]
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rFieldDecls1 :: RuleExpr (Seq.Seq (Strictness, [Id], Type))
rFieldDecls1 = ruleExpr
    [ alt $ varA @"fielddecl" <^> tokA @"," <^> varA @"fielddecls1"
        <:> semAct \(fielddecl :* _ :* fielddecls1 :* HNil) ->
            [||$$(fielddecl) Seq.:<| $$(fielddecls1)||]
    , alt $ varA @"fielddecl"
        <:> semAct \(fielddecl :* HNil) ->
            [||pure $$(fielddecl)||]
    ]

rAbctype :: RuleExpr (Strictness, Type)
rAbctype = ruleExpr
    [ alt $ varA @"btype"
        <:> semAct \(btype :* HNil) ->
            [||(Unstrict, $$(btype))||]
    , alt $ varA @"!" <^> varA @"atype"
        <:> semAct \(_ :* atype :* HNil) ->
            [||(Strict, $$(atype))||]
    ]

rActypes :: RuleExpr (Seq.Seq (Strictness, Type))
rActypes = ruleExpr
    [ alt $ varA @"actype" <^> varA @"actypes"
        <:> semAct \(actype :* actypes :* HNil) ->
            [||$$(actype) Seq.:<| $$(actypes)||]
    , eps
        $ semAct \HNil ->
            [||Seq.empty||]
    ]

rActype :: RuleExpr (Strictness, Type)
rActype = ruleExpr
    [ alt $ varA @"!" <^> varA @"atype"
        <:> semAct \(_ :* atype :* HNil) ->
            [||(Strict, $$(atype))||]
    , alt $ varA @"atype"
        <:> semAct \(atype :* HNil) ->
            [||(Unstrict, $$(atype))||]
    ]

rNewConstr :: RuleExpr Constr
rNewConstr = ruleExpr
    [ alt $ varA @"con" <^> varA @"{" <^> varA @"var" <^> tokA @"::" <^> varA @"type" <^> varA @"}"
        <:> semAct \(con :* _ :* var :* _ :* ty :* _ :* HNil) ->
            [||ConstrWithFields $$(con) [(Unstrict, [$$(var)], $$(ty))]||]
    , alt $ varA @"con" <^> varA @"atype"
        <:> semAct \(con :* atype :* HNil) ->
            [||ConstrApp $$(con) [(Unstrict, $$(atype))]||]
    ]

rFieldDecl :: RuleExpr (Strictness, [Id], Type)
rFieldDecl = ruleExpr
    [ alt $ varA @"vars" <^> tokA @"::" <^> varA @"type"
        <:> semAct \(vars :* _ :* ty :* HNil) ->
            [||(Unstrict, toList $$(vars), $$(ty))||]
    , alt $ varA @"vars" <^> tokA @"::" <^> varA @"!" <^> varA @"atype"
        <:> semAct \(vars :* _ :* _ :* atype :* HNil) ->
            [||(Strict, toList $$(vars), $$(atype))||]
    ]

rDeriving :: RuleExpr Deriving
rDeriving = ruleExpr
    [ alt $ tokA @"deriving" <^> varA @"dclass"
        <:> semAct \(_ :* dclass :* HNil) ->
            [||Deriving [$$(dclass)]||]
    , alt $ tokA @"deriving" <^> tokA @"(" <^> varA @"dclasses" <^> tokA @")"
        <:> semAct \(_ :* _ :* dclasses :* _ :* HNil) ->
            [||Deriving $$(dclasses)||]
    ]

rDclasses :: RuleExpr [Type]
rDclasses = ruleExpr
    [ alt $ varA @"dclasses1"
        <:> semAct \(dclasses1 :* HNil) ->
            [||toList $$(dclasses1)||]
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rDclasses1 :: RuleExpr (Seq.Seq Type)
rDclasses1 = ruleExpr
    [ alt $ varA @"dclass" <^> tokA @"," <^> varA @"dclasses1"
        <:> semAct \(dclass :* _ :* dclasses1 :* HNil) ->
            [||$$(dclass) Seq.:<| $$(dclasses1)||]
    , alt $ varA @"dclass"
        <:> semAct \(dclass :* HNil) ->
            [||pure $$(dclass)||]
    ]

rDclass :: RuleExpr Type
rDclass = ruleExpr
    [ alt $ varA @"qtycon"
        <:> semAct \(qtycon :* HNil) ->
            [||TypeId $$(qtycon)||]
    ]

rInst :: RuleExpr Type
rInst = ruleExpr
    [ alt $ varA @"qtycon"
        <:> semAct \(qtycon :* HNil) ->
            [||TypeId $$(qtycon)||]
    , alt $ tokA @"(" <^> varA @"gtycon" <^> varA @"tyvars" <^> tokA @")"
        <:> semAct \(_ :* gtycon :* tyvars :* _ :* HNil) ->
            [||TypeApp $$(gtycon) (toList $$(tyvars))||]
    , alt $ tokA @"(" <^> varA @"tyvars2" <^> tokA @")"
        <:> semAct \(_ :* tyvars2 :* _ :* HNil) ->
            [||TypeTuple (toList $$(tyvars2))||]
    , alt $ tokA @"(" <^> varA @"tyvar" <^> tokA @")"
        <:> semAct \(_ :* tyvar :* _ :* HNil) ->
            [||TypeId (nonQualifiedId $$(tyvar))||]
    , alt $ tokA @"[" <^> varA @"tyvar" <^> tokA @"]"
        <:> semAct \(_ :* tyvar :* _ :* HNil) ->
            [||TypeList (TypeId (nonQualifiedId $$(tyvar)))||]
    , alt $ tokA @"(" <^> varA @"tyvar" <^> tokA @"->" <^> varA @"tyvar" <^> tokA @")"
        <:> semAct \(_ :* tyvar1 :* _ :* tyvar2 :* _ :* HNil) ->
            [||TypeArrow (TypeId (nonQualifiedId $$(tyvar1))) (TypeId (nonQualifiedId $$(tyvar2)))||]
    ]

rTyVars2 :: RuleExpr (Seq.Seq Type)
rTyVars2 = ruleExpr
    [ alt $ varA @"tyvar" <^> tokA @"," <^> varA @"tyvars2"
        <:> semAct \(tyvar :* _ :* tyvars2 :* HNil) ->
            [||TypeId (nonQualifiedId $$(tyvar)) Seq.:<| $$(tyvars2)||]
    , alt $ varA @"tyvar" <^> tokA @"," <^> varA @"tyvar"
        <:> semAct \(tyvar1 :* _ :* tyvar2 :* HNil) ->
            [||Seq.fromList [TypeId (nonQualifiedId $$(tyvar1)), TypeId (nonQualifiedId $$(tyvar2))]||]
    ]

rFdecl :: RuleExpr Decl
rFdecl = ruleExpr
    [ alt $ tokA @"import" <^> varA @"callconv" <^> varA @"safetyopt" <^> varA @"impent" <^> varA @"var" <^> tokA @"::" <^> varA @"ftype"
        <:> semAct \(_ :* callconv :* safetyopt :* impent :* var :* _ :* ftype :* HNil) ->
            [||DeclForeignImport $$(callconv) $$(safetyopt) $$(impent) $$(var) $$(ftype)||]
    , alt $ varA @"exportid" <^> varA @"callconv" <^> varA @"expent" <^> varA @"var" <^> tokA @"::" <^> varA @"ftype"
        <:> semAct \(_ :* callconv :* expent :* var :* _ :* ftype :* HNil) ->
            [||DeclForeignExport $$(callconv) $$(expent) $$(var) $$(ftype)||]
    ]

rCallConv :: RuleExpr ForeignCallConv
rCallConv = ruleExpr
    [ alt $ varA @"varid"
        <:> semActM \(varid :* HNil) ->
            [||case $$(varid) of
                Id bs | bs == Char8.pack "ccall" ->
                    pure ForeignCallCcall
                Id bs | bs == Char8.pack "stdcall" ->
                    pure ForeignCallStdcall
                Id bs | bs == Char8.pack "cplusplus" ->
                    pure ForeignCallCplusplus
                Id bs | bs == Char8.pack "jvm" ->
                    pure ForeignCallJvm
                Id bs | bs == Char8.pack "dotnet" ->
                    pure ForeignCallDotnet
                _ ->
                    failAction
            ||]
    ]

rSafetyOpt :: RuleExpr (Maybe Safety)
rSafetyOpt = ruleExpr
    [ alt $ varA @"safety"
        <:> semAct \(safety :* HNil) ->
            [||Just $$(safety)||]
    , eps
        $ semAct \HNil ->
            [||Nothing||]
    ]

rImpent :: RuleExpr (Maybe String)
rImpent = ruleExpr
    [ alt $ varA @"string"
        <:> semAct \(string :* HNil) ->
            [||Just $$(string)||]
    , eps
        $ semAct \HNil ->
            [||Nothing||]
    ]

rExpent :: RuleExpr (Maybe String)
rExpent = ruleExpr
    [ alt $ varA @"string"
        <:> semAct \(string :* HNil) ->
            [||Just $$(string)||]
    , eps
        $ semAct \HNil ->
            [||Nothing||]
    ]

rSafety :: RuleExpr Safety
rSafety = ruleExpr
    [ alt $ varA @"varid"
        <:> semActM \(varid :* HNil) ->
            [||case $$(varid) of
                Id bs | bs == Char8.pack "safe" ->
                    pure Safe
                Id bs | bs == Char8.pack "unsafe" ->
                    pure Unsafe
                _ ->
                    failAction
            ||]
    ]

rFtype :: RuleExpr Type
rFtype = ruleExpr
    [ alt $ varA @"fatype" <^> tokA @"->" <^> varA @"ftype"
        <:> semAct \(fatype :* _ :* ftype :* HNil) ->
            [||TypeArrow $$(fatype) $$(ftype)||]
    , alt $ varA @"frtype"
        <:> semAct \(frtype :* HNil) ->
            frtype
    ]

rFrtype :: RuleExpr Type
rFrtype = ruleExpr
    [ alt $ varA @"fatype"
        <:> semAct \(fatype :* HNil) ->
            fatype
    , alt $ tokA @"(" <^> tokA @")"
        <:> semAct \(_ :* _ :* HNil) ->
            [||TypeId (nonQualifiedId (mkId "()"))||]
    ]

rFatype :: RuleExpr Type
rFatype = ruleExpr
    [ alt $ varA @"qtycon" <^> varA @"atypes"
        <:> semAct \(qtycon :* atypes :* HNil) ->
            [||TypeApp (TypeId $$(qtycon)) (toList $$(atypes))||]
    ]

rFunlhs :: RuleExpr (Id, Seq.Seq Pat)
rFunlhs = ruleExpr
    [ alt $ varA @"var" <^> varA @"apats1"
        <:> semAct \(var :* apats1 :* HNil) ->
            [||($$(var), $$(apats1))||]
    , alt $ varA @"pat" <^> varA @"varop" <^> varA @"pat"
        <:> semAct \(pat1 :* varop :* pat2 :* HNil) ->
            [||($$(varop), Seq.fromList [$$(pat1), $$(pat2)])||]
    , alt $ tokA @"(" <^> varA @"funlhs" <^> tokA @")" <^> varA @"apats1"
        <:> semAct \(_ :* funlhs :* _ :* apats1 :* HNil) ->
            [||case $$(funlhs) of
                (v, pats) -> (v, pats <> $$(apats1))
            ||]
    ]

rApats1 :: RuleExpr (Seq.Seq Pat)
rApats1 = ruleExpr
    [ alt $ varA @"apat" <^> varA @"apats1"
        <:> semAct \(apat :* apats1 :* HNil) ->
            [||$$(apat) Seq.:<| $$(apats1)||]
    , alt $ varA @"apat"
        <:> semAct \(apat :* HNil) ->
            [||pure $$(apat)||]
    ]

rRhs :: RuleExpr Rhs
rRhs = ruleExpr
    [ alt $ tokA @"=" <^> varA @"exp" <^> varA @"wheredeclsopt"
        <:> semAct \(_ :* exp :* decls :* HNil) ->
            [||Rhs [([], $$(exp))] $$(decls)||]
    , alt $ varA @"gdrhs" <^> varA @"wheredeclsopt"
        <:> semAct \(gdrhs :* decls :* HNil) ->
            [||Rhs (toList $$(gdrhs)) $$(decls)||]
    ]

rWhereDeclsOpt :: RuleExpr [Decl]
rWhereDeclsOpt = ruleExpr
    [ alt $ tokA @"where" <^> varA @"decls"
        <:> semAct \(_ :* decls :* HNil) ->
            decls
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rGdrhs :: RuleExpr (Seq.Seq ([Guard], Exp))
rGdrhs = ruleExpr
    [ alt $ varA @"guards" <^> tokA @"=" <^> varA @"exp" <^> varA @"gdrhs"
        <:> semAct \(guards :* _ :* exp :* gdrhs :* HNil) ->
            [||($$(guards), $$(exp)) Seq.:<| $$(gdrhs)||]
    , alt $ varA @"guards" <^> tokA @"=" <^> varA @"exp"
        <:> semAct \(guards :* _ :* exp :* HNil) ->
            [||pure ($$(guards), $$(exp))||]
    ]

rGuards :: RuleExpr [Guard]
rGuards = ruleExpr
    [ alt $ varA @"guards1"
        <:> semAct \(guards1 :* HNil) ->
            [||toList $$(guards1)||]
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rGuards1 :: RuleExpr (Seq.Seq Guard)
rGuards1 = ruleExpr
    [ alt $ varA @"guard" <^> tokA @"," <^> varA @"guards1"
        <:> semAct \(guard :* _ :* guards1 :* HNil) ->
            [||$$(guard) Seq.:<| $$(guards1)||]
    , alt $ varA @"guard"
        <:> semAct \(guard :* HNil) ->
            [||pure $$(guard)||]
    ]

rGuard :: RuleExpr Guard
rGuard = ruleExpr
    [ alt $ varA @"pat" <^> tokA @"<-" <^> varA @"infixexp"
        <:> semAct \(pat :* _ :* infixexp :* HNil) ->
            [||GuardPat $$(pat) $$(infixexp)||]
    , alt $ tokA @"let" <^> varA @"decls"
        <:> semAct \(_ :* decls :* HNil) ->
            [||GuardLet $$(decls)||]
    , alt $ varA @"infixexp"
        <:> semAct \(infixexp :* HNil) ->
            [||GuardExp $$(infixexp)||]
    ]

rExp :: RuleExpr Exp
rExp = ruleExpr
    [ alt $ varA @"infixexp" <^> tokA @"::" <^> varA @"contextopt" <^> varA @"type"
        <:> semAct \(infixexp :* _ :* contextopt :* ty :* HNil) ->
            [||ExpSig $$(infixexp) $$(contextopt) $$(ty)||]
    , alt $ varA @"infixexp"
        <:> semAct \(infixexp :* HNil) ->
            infixexp
    ]

rInfixExp :: RuleExpr Exp
rInfixExp = ruleExpr
    [ alt $ tokA @"-" <^> varA @"infixexp"
        <:> semAct \(_ :* infixexp :* HNil) ->
            [||ExpMinus $$(infixexp)||]
    , alt $ varA @"lexp" <^> varA @"qop" <^> varA @"infixexp"
        <:> semAct \(lexp :* qop :* infixexp :* HNil) ->
            [||ExpInfixApp $$(lexp) $$(qop) $$(infixexp)||]
    , alt $ varA @"lexp"
        <:> semAct \(lexp :* HNil) ->
            lexp
    ]

rLexp :: RuleExpr Exp
rLexp = ruleExpr
    [ alt $ tokA @"\\" <^> varA @"apats1" <^> tokA @"->" <^> varA @"exp"
        <:> semAct \(_ :* apats1 :* _ :* exp :* HNil) ->
            [||ExpLambda (toList $$(apats1)) $$(exp)||]
    , alt $ tokA @"let" <^> varA @"decls" <^> tokA @"in" <^> varA @"exp"
        <:> semAct \(_ :* decls :* _ :* exp :* HNil) ->
            [||ExpLet $$(decls) $$(exp)||]
    , alt $ tokA @"if" <^> varA @"exp" <^> varA @"semiopt" <^> tokA @"then" <^> varA @"exp" <^> varA @"semiopt" <^> tokA @"else" <^> varA @"exp"
        <:> semAct \(_ :* exp1 :* _ :* _ :* exp2 :* _ :* _ :* exp3 :* HNil) ->
            [||ExpIf $$(exp1) $$(exp2) $$(exp3)||]
    , alt $ tokA @"case" <^> varA @"exp" <^> tokA @"of" <^> varA @"casealts"
        <:> semAct \(_ :* exp :* _ :* casealts :* HNil) ->
            [||ExpCase $$(exp) $$(casealts)||]
    , alt $ tokA @"do" <^> varA @"dostmts"
        <:> semAct \(_ :* dostmts :* HNil) ->
            [||case $$(dostmts) of
                (stmts, exp) -> ExpDo stmts exp
            ||]
    , alt $ varA @"fexp"
        <:> semAct \(fexp :* HNil) ->
            fexp
    ]

rSemiopt :: RuleExpr ()
rSemiopt = ruleExpr
    [ alt $ varA @";"
        <:> semAct \(_ :* HNil) ->
            [||()||]
    , eps
        $ semAct \HNil ->
            [||()||]
    ]

rFexp :: RuleExpr Exp
rFexp = ruleExpr
    [ alt $ varA @"aexp" <^> varA @"aexps"
        <:> semAct \(aexp :* aexps :* HNil) ->
            [||ExpApp $$(aexp) (toList $$(aexps))||]
    ]

rAexps :: RuleExpr (Seq.Seq Exp)
rAexps = ruleExpr
    [ alt $ varA @"aexp" <^> varA @"aexps"
        <:> semAct \(aexp :* aexps :* HNil) ->
            [||$$(aexp) Seq.:<| $$(aexps)||]
    , eps
        $ semAct \HNil ->
            [||Seq.empty||]
    ]

rAexp :: RuleExpr Exp
rAexp = ruleExpr
    [ alt $ varA @"literal"
        <:> semAct \(literal :* HNil) ->
            [||ExpLit $$(literal)||]
    , alt $ tokA @"(" <^> varA @"exp" <^> tokA @")"
        <:> semAct \(_ :* exp :* _ :* HNil) ->
            exp
    , alt $ tokA @"(" <^> varA @"exps2" <^> tokA @")"
        <:> semAct \(_ :* exps2 :* _ :* HNil) ->
            [||ExpTuple (toList $$(exps2))||]
    , alt $ tokA @"[" <^> varA @"exps1" <^> tokA @"]"
        <:> semAct \(_ :* exps1 :* _ :* HNil) ->
            [||ExpList (toList $$(exps1))||]
    , alt $ tokA @"[" <^> varA @"exp" <^> varA @"cexpopt" <^> tokA @".." <^> varA @"expopt" <^> tokA @"]"
        <:> semAct \(_ :* exp :* cexpopt :* _ :* expopt :* _ :* HNil) ->
            [||ExpListRange $$(exp) $$(cexpopt) $$(expopt)||]
    , alt $ tokA @"[" <^> varA @"exp" <^> tokA @"|" <^> varA @"quals1" <^> tokA @"]"
        <:> semAct \(_ :* exp :* _ :* quals1 :* _ :* HNil) ->
            [||ExpListComp $$(exp) (toList $$(quals1))||]
    , alt $ tokA @"(" <^> varA @"infixexp" <^> varA @"qop" <^> tokA @")"
        <:> semAct \(_ :* infixexp :* qop :* _ :* HNil) ->
            [||ExpSection (Just $$(infixexp)) $$(qop) Nothing||]
    -- `"(" exp ")"` includes `"(" "-" infixexp ")"`
    , alt $ tokA @"(" <^> varA @"qop" <^> varA @"infixexp" <^> tokA @")"
        <:> semAct \(_ :* qop :* infixexp :* _ :* HNil) ->
            [||ExpSection Nothing $$(qop) (Just $$(infixexp))||]
    , alt $ varA @"qcon" <^> varA @"{" <^> varA @"fbinds" <^> tokA @"}"
        <:> semAct \(qcon :* _ :* fbinds :* _ :* HNil) ->
            [||ExpRecordCon $$(qcon) $$(fbinds)||]
    , alt $ varA @"aexp" <^> varA @"{" <^> varA @"fbinds" <^> tokA @"}"
        <:> semAct \(aexp :* _ :* fbinds :* _ :* HNil) ->
            [||ExpRecordUpdate $$(aexp) $$(fbinds)||]
    , alt $ varA @"qvar"
        <:> semAct \(qvar :* HNil) ->
            [||ExpId $$(qvar)||]
    , alt $ varA @"gcon"
        <:> semAct \(gcon :* HNil) ->
            [||case $$(gcon) of
                GconId con ->
                    ExpId con
                GconTuple i ->
                    ExpTupleCon i
            ||]
    ]

rExps2 :: RuleExpr (Seq.Seq Exp)
rExps2 = ruleExpr
    [ alt $ varA @"exp" <^> tokA @"," <^> varA @"exps1"
        <:> semAct \(exp :* _ :* exps1 :* HNil) ->
            [||$$(exp) Seq.:<| $$(exps1)||]
    ]

rExps1 :: RuleExpr (Seq.Seq Exp)
rExps1 = ruleExpr
    [ alt $ varA @"exp" <^> tokA @"," <^> varA @"exps1"
        <:> semAct \(exp :* _ :* exps1 :* HNil) ->
            [||$$(exp) Seq.:<| $$(exps1)||]
    , alt $ varA @"exp"
        <:> semAct \(exp :* HNil) ->
            [||pure $$(exp)||]
    ]

rCexpOpt :: RuleExpr (Maybe Exp)
rCexpOpt = ruleExpr
    [ alt $ tokA @"," <^> varA @"exp"
        <:> semAct \(_ :* exp :* HNil) ->
            [||Just $$(exp)||]
    , eps
        $ semAct \HNil ->
            [||Nothing||]
    ]

rExpOpt :: RuleExpr (Maybe Exp)
rExpOpt = ruleExpr
    [ alt $ varA @"exp"
        <:> semAct \(exp :* HNil) ->
            [||Just $$(exp)||]
    , eps
        $ semAct \HNil ->
            [||Nothing||]
    ]

rQuals1 :: RuleExpr (Seq.Seq Guard)
rQuals1 = ruleExpr
    [ alt $ varA @"qual" <^> tokA @"," <^> varA @"quals1"
        <:> semAct \(qual :* _ :* quals1 :* HNil) ->
            [||$$(qual) Seq.:<| $$(quals1)||]
    , alt $ varA @"qual"
        <:> semAct \(qual :* HNil) ->
            [||pure $$(qual)||]
    ]

rFbinds :: RuleExpr [(QualifiedId, Exp)]
rFbinds = ruleExpr
    [ alt $ varA @"fbinds1"
        <:> semAct \(fbinds1 :* HNil) ->
            [||toList $$(fbinds1)||]
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rFbinds1 :: RuleExpr (Seq.Seq (QualifiedId, Exp))
rFbinds1 = ruleExpr
    [ alt $ varA @"fbind" <^> tokA @"," <^> varA @"fbinds1"
        <:> semAct \(fbind :* _ :* fbinds1 :* HNil) ->
            [||$$(fbind) Seq.:<| $$(fbinds1)||]
    , alt $ varA @"fbind"
        <:> semAct \(fbind :* HNil) ->
            [||pure $$(fbind)||]
    ]

rQual :: RuleExpr Guard
rQual = ruleExpr
    [ alt $ varA @"pat" <^> tokA @"<-" <^> varA @"exp"
        <:> semAct \(pat :* _ :* exp :* HNil) ->
            [||GuardPat $$(pat) $$(exp)||]
    , alt $ tokA @"let" <^> varA @"decls"
        <:> semAct \(_ :* decls :* HNil) ->
            [||GuardLet $$(decls)||]
    , alt $ varA @"exp"
        <:> semAct \(exp :* HNil) ->
            [||GuardExp $$(exp)||]
    ]

rCaseAlts :: RuleExpr [CaseAlt]
rCaseAlts = ruleExpr
    [ alt $ varA @"{" <^> varA @"alts" <^> varA @"}"
        <:> semAct \(_ :* alts :* _ :* HNil) ->
            [||toList $$(alts)||]
    , alt $ varA @"impbo" <^> varA @"alts" <^> varA @"impbc"
        <:> semAct \(_ :* alts :* _ :* HNil) ->
            [||toList $$(alts)||]
    ]

rAlts :: RuleExpr (Seq.Seq CaseAlt)
rAlts = ruleExpr
    [ alt $ varA @"alt" <^> varA @";" <^> varA @"alts"
        <:> semAct \(alt :* _ :* alts :* HNil) ->
            [||$$(alt) <> $$(alts)||]
    , alt $ varA @"alt"
        <:> semAct \(alt :* HNil) ->
            alt
    ]

rAlt :: RuleExpr (Seq.Seq CaseAlt)
rAlt = ruleExpr
    [ alt $ varA @"pat" <^> tokA @"->" <^> varA @"exp" <^> varA @"wheredeclsopt"
        <:> semAct \(pat :* _ :* exp :* decls :* HNil) ->
            [||pure $ CaseAlt $$(pat) [([], $$(exp))] $$(decls)||]
    , alt $ varA @"pat" <^> varA @"gdpat" <^> varA @"wheredeclsopt"
        <:> semAct \(pat :* gdpat :* decls :* HNil) ->
            [||pure $ CaseAlt $$(pat) (toList $$(gdpat)) $$(decls)||]
    , eps
        $ semAct \HNil ->
            [||Seq.empty||]
    ]

rGdpat :: RuleExpr (Seq.Seq ([Guard], Exp))
rGdpat = ruleExpr
    [ alt $ varA @"guards" <^> tokA @"->" <^> varA @"exp" <^> varA @"gdpat"
        <:> semAct \(guards :* _ :* exp :* gdpat :* HNil) ->
            [||($$(guards), $$(exp)) Seq.:<| $$(gdpat)||]
    , alt $ varA @"guards" <^> tokA @"->" <^> varA @"exp"
        <:> semAct \(guards :* _ :* exp :* HNil) ->
            [||pure ($$(guards), $$(exp))||]
    ]

rDoStmts :: RuleExpr ([Stmt], Exp)
rDoStmts = ruleExpr
    [ alt $ varA @"{" <^> varA @"stmts" <^> varA @"}"
        <:> semAct \(_ :* stmts :* _ :* HNil) ->
            stmts
    , alt $ varA @"impbo" <^> varA @"stmts" <^> varA @"impbc"
        <:> semAct \(_ :* stmts :* _ :* HNil) ->
            stmts
    ]

rStmts :: RuleExpr ([Stmt], Exp)
rStmts = ruleExpr
    [ alt $ varA @"stmts0" <^> varA @"exp" <^> varA @"semiopt"
        <:> semAct \(stmts0 :* exp :* _ :* HNil) ->
            [||(toList $$(stmts0), $$(exp))||]
    ]

rStmts0 :: RuleExpr (Seq.Seq Stmt)
rStmts0 = ruleExpr
    [ alt $ varA @"stmt" <^> varA @"stmts0"
        <:> semAct \(stmt :* stmts0 :* HNil) ->
            [||$$(stmt) <> $$(stmts0)||]
    , eps
        $ semAct \HNil ->
            [||Seq.empty||]
    ]

rStmt :: RuleExpr (Seq.Seq Stmt)
rStmt = ruleExpr
    [ alt $ varA @"exp" <^> varA @";"
        <:> semAct \(exp :* _ :* HNil) ->
            [||pure $ StmtExp $$(exp)||]
    , alt $ varA @"pat" <^> tokA @"<-" <^> varA @"exp" <^> varA @";"
        <:> semAct \(pat :* _ :* exp :* _ :* HNil) ->
            [||pure $ StmtPat $$(pat) $$(exp)||]
    , alt $ tokA @"let" <^> varA @"decls" <^> varA @";"
        <:> semAct \(_ :* decls :* _ :* HNil) ->
            [||pure $ StmtLet $$(decls)||]
    , alt $ varA @";"
        <:> semAct \(_ :* HNil) ->
            [||Seq.empty||]
    ]

rFbind :: RuleExpr (QualifiedId, Exp)
rFbind = ruleExpr
    [ alt $ varA @"qvar" <^> tokA @"=" <^> varA @"exp"
        <:> semAct \(qvar :* _ :* exp :* HNil) ->
            [||($$(qvar), $$(exp))||]
    ]

rPat :: RuleExpr Pat
rPat = ruleExpr
    [ alt $ varA @"lpat" <^> varA @"qconop" <^> varA @"pat"
        <:> semAct \(lpat :* qconop :* pat :* HNil) ->
            [||PatInfixApp $$(lpat) $$(qconop) $$(pat)||]
    , alt $ varA @"lpat"
        <:> semAct \(lpat :* HNil) ->
            lpat
    ]

rLpat :: RuleExpr Pat
rLpat = ruleExpr
    [ alt $ tokA @"-" <^> varA @"integer"
        <:> semAct \(_ :* integer :* HNil) ->
            [||PatMinusInteger $$(integer)||]
    , alt $ tokA @"-" <^> varA @"float"
        <:> semAct \(_ :* float :* HNil) ->
            [||PatMinusFloat $$(float)||]
    , alt $ varA @"gcon" <^> varA @"apats1"
        <:> semAct \(gcon :* apats1 :* HNil) ->
            [||PatApp $$(gcon) (toList $$(apats1))||]
    , alt $ varA @"apat"
        <:> semAct \(apat :* HNil) ->
            apat
    ]

rApat :: RuleExpr Pat
rApat = ruleExpr
    [ alt $ varA @"var" <^> tokA @"@" <^> varA @"apat"
        <:> semAct \(var :* _ :* apat :* HNil) ->
            [||PatId $$(var) (Just $$(apat))||]
    , alt $ varA @"var"
        <:> semAct \(var :* HNil) ->
            [||PatId $$(var) Nothing||]
    , alt $ varA @"literal"
        <:> semAct \(literal :* HNil) ->
            [||PatLit $$(literal)||]
    , alt $ tokA @"_"
        <:> semAct \(_ :* HNil) ->
            [||PatWildcard||]
    , alt $ tokA @"(" <^> varA @"pat" <^> tokA @")"
        <:> semAct \(_ :* pat :* _ :* HNil) ->
            pat
    , alt $ tokA @"(" <^> varA @"pats2" <^> tokA @")"
        <:> semAct \(_ :* pats2 :* _ :* HNil) ->
            [||PatTuple (toList $$(pats2))||]
    , alt $ tokA @"[" <^> varA @"pats1" <^> tokA @"]"
        <:> semAct \(_ :* pats1 :* _ :* HNil) ->
            [||PatList (toList $$(pats1))||]
    , alt $ tokA @"~" <^> varA @"apat"
        <:> semAct \(_ :* apat :* HNil) ->
            [||PatLazy $$(apat)||]
    , alt $ varA @"qcon" <^> varA @"{" <^> varA @"fpats" <^> varA @"}"
        <:> semAct \(qcon :* _ :* fpats :* _ :* HNil) ->
            [||PatRecord $$(qcon) $$(fpats)||]
    , alt $ varA @"gcon"
        <:> semAct \(gcon :* HNil) ->
            [||PatCon $$(gcon)||]
    ]

rPats2 :: RuleExpr (Seq.Seq Pat)
rPats2 = ruleExpr
    [ alt $ varA @"pat" <^> tokA @"," <^> varA @"pats2"
        <:> semAct \(pat :* _ :* pats2 :* HNil) ->
            [||$$(pat) Seq.:<| $$(pats2)||]
    , alt $ varA @"pat" <^> tokA @"," <^> varA @"pat"
        <:> semAct \(pat1 :* _ :* pat2 :* HNil) ->
            [||Seq.fromList [$$(pat1), $$(pat2)]||]
    ]

rPats1 :: RuleExpr (Seq.Seq Pat)
rPats1 = ruleExpr
    [ alt $ varA @"pat" <^> tokA @"," <^> varA @"pats1"
        <:> semAct \(pat :* _ :* pats1 :* HNil) ->
            [||$$(pat) Seq.:<| $$(pats1)||]
    , alt $ varA @"pat"
        <:> semAct \(pat :* HNil) ->
            [||Seq.fromList [$$(pat)]||]
    ]

rFpats :: RuleExpr [(QualifiedId, Pat)]
rFpats = ruleExpr
    [ alt $ varA @"fpats1"
        <:> semAct \(fpats1 :* HNil) ->
            [||toList $$(fpats1)||]
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rFpats1 :: RuleExpr (Seq.Seq (QualifiedId, Pat))
rFpats1 = ruleExpr
    [ alt $ varA @"fpat" <^> tokA @"," <^> varA @"fpats1"
        <:> semAct \(fpat :* _ :* fpats1 :* HNil) ->
            [||$$(fpat) Seq.:<| $$(fpats1)||]
    , alt $ varA @"fpat"
        <:> semAct \(fpat :* HNil) ->
            [||pure $$(fpat)||]
    ]

rFpat :: RuleExpr (QualifiedId, Pat)
rFpat = ruleExpr
    [ alt $ varA @"qvar" <^> tokA @"=" <^> varA @"pat"
        <:> semAct \(qvar :* _ :* pat :* HNil) ->
            [||($$(qvar), $$(pat))||]
    ]

rGcon :: RuleExpr Gcon
rGcon = ruleExpr
    [ alt $ tokA @"(" <^> tokA @")"
        <:> semAct \(_ :* _ :* HNil) ->
            [||GconId $ nonQualifiedId (mkId "()")||]
    , alt $ tokA @"[" <^> tokA @"]"
        <:> semAct \(_ :* _ :* HNil) ->
            [||GconId $ nonQualifiedId (mkId "[]")||]
    , alt $ tokA @"(" <^> varA @"commas1" <^> tokA @")"
        <:> semAct \(_ :* i :* _ :* HNil) ->
            [||GconTuple $$(i)||]
    , alt $ varA @"qcon"
        <:> semAct \(qcon :* HNil) ->
            [||GconId $$(qcon)||]
    ]

rVar :: RuleExpr Id
rVar = ruleExpr
    [ alt $ varA @"varid"
        <:> semAct \(varid :* HNil) ->
            varid
    , alt $ tokA @"(" <^> varA @"varsym" <^> tokA @")"
        <:> semAct \(_ :* varsym :* _ :* HNil) ->
            varsym
    ]

rQvar :: RuleExpr QualifiedId
rQvar = ruleExpr
    [ alt $ varA @"qvarid"
        <:> semAct \(qvarid :* HNil) ->
            qvarid
    , alt $ tokA @"(" <^> varA @"qvarsym" <^> tokA @")"
        <:> semAct \(_ :* qvarsym :* _ :* HNil) ->
            qvarsym
    ]

rCon :: RuleExpr Id
rCon = ruleExpr
    [ alt $ varA @"conid"
        <:> semAct \(conid :* HNil) ->
            conid
    , alt $ tokA @"(" <^> varA @"consym" <^> tokA @")"
        <:> semAct \(_ :* consym :* _ :* HNil) ->
            consym
    ]

rQcon :: RuleExpr QualifiedId
rQcon = ruleExpr
    [ alt $ varA @"qconid"
        <:> semAct \(qconid :* HNil) ->
            qconid
    , alt $ tokA @"(" <^> varA @"gconsym" <^> tokA @")"
        <:> semAct \(_ :* qconsym :* _ :* HNil) ->
            qconsym
    ]

rVarOp :: RuleExpr Id
rVarOp = ruleExpr
    [ alt $ varA @"varsym"
        <:> semAct \(varsym :* HNil) ->
            varsym
    , alt $ tokA @"`" <^> varA @"varid" <^> tokA @"`"
        <:> semAct \(_ :* varid :* _ :* HNil) ->
            varid
    ]

rQvarOp :: RuleExpr QualifiedId
rQvarOp = ruleExpr
    [ alt $ varA @"qvarsym"
        <:> semAct \(qvarsym :* HNil) ->
            qvarsym
    , alt $ tokA @"`" <^> varA @"qvarid" <^> tokA @"`"
        <:> semAct \(_ :* qvarid :* _ :* HNil) ->
            qvarid
    ]

rConOp :: RuleExpr Id
rConOp = ruleExpr
    [ alt $ varA @"consym"
        <:> semAct \(consym :* HNil) ->
            consym
    , alt $ tokA @"`" <^> varA @"conid" <^> tokA @"`"
        <:> semAct \(_ :* conid :* _ :* HNil) ->
            conid
    ]

rQconOp :: RuleExpr QualifiedId
rQconOp = ruleExpr
    [ alt $ varA @"gconsym"
        <:> semAct \(qconsym :* HNil) ->
            qconsym
    , alt $ tokA @"`" <^> varA @"qconid" <^> tokA @"`"
        <:> semAct \(_ :* qconid :* _ :* HNil) ->
            qconid
    ]

rOp :: RuleExpr Id
rOp = ruleExpr
    [ alt $ varA @"varop"
        <:> semAct \(varop :* HNil) ->
            varop
    , alt $ varA @"conop"
        <:> semAct \(conop :* HNil) ->
            conop
    ]

rQop :: RuleExpr QualifiedId
rQop = ruleExpr
    [ alt $ varA @"qvarop"
        <:> semAct \(qvarop :* HNil) ->
            qvarop
    , alt $ varA @"qconop"
        <:> semAct \(qconop :* HNil) ->
            qconop
    ]

rGconsym :: RuleExpr QualifiedId
rGconsym = ruleExpr
    [ alt $ tokA @":"
        <:> semAct \(_ :* HNil) ->
            [||nonQualifiedId (mkId ":")||]
    , alt $ varA @"qconsym"
        <:> semAct \(qconsym :* HNil) ->
            qconsym
    ]

rTyvar :: RuleExpr Id
rTyvar = ruleExpr
    [ alt $ varA @"varid"
        <:> semAct \(varid :* HNil) ->
            varid
    ]

rTycon :: RuleExpr Id
rTycon = ruleExpr
    [ alt $ varA @"conid"
        <:> semAct \(conid :* HNil) ->
            conid
    ]

rQtycon :: RuleExpr QualifiedId
rQtycon = ruleExpr
    [ alt $ varA @"qconid"
        <:> semAct \(qconid :* HNil) ->
            qconid
    ]

rVarId :: RuleExpr Id
rVarId = ruleExpr
    [ alt $ varA @"qvarid"
        <:> semActM \(qvarid :* HNil) ->
            [||case $$(qvarid) of
                QualifiedId (ModId []) varid ->
                    pure varid
                _ ->
                    failAction
            ||]
    ]

rVarSym :: RuleExpr Id
rVarSym = ruleExpr
    [ alt $ varA @"qvarsym"
        <:> semActM \(qvarsym :* HNil) ->
            [||case $$(qvarsym) of
                QualifiedId (ModId []) varsym ->
                    pure varsym
                _ ->
                    failAction
            ||]
    ]

rConId :: RuleExpr Id
rConId = ruleExpr
    [ alt $ varA @"qconid"
        <:> semActM \(qconid :* HNil) ->
            [||case $$(qconid) of
                QualifiedId (ModId []) conid ->
                    pure conid
                _ ->
                    failAction
            ||]
    ]

rConSym :: RuleExpr Id
rConSym = ruleExpr
    [ alt $ varA @"qconsym"
        <:> semActM \(qconsym :* HNil) ->
            [||case $$(qconsym) of
                QualifiedId (ModId []) consym ->
                    pure consym
                _ ->
                    failAction
            ||]
    ]

rQvarId :: RuleExpr QualifiedId
rQvarId = ruleExpr
    [ alt $ tokA @"qvarid"
        <:> semAct \(qvarid :* HNil) ->
            [||case $$(qvarid) of
                TokQualifiedVarId modid varid ->
                    QualifiedId (coerce modid) (Id varid)
                _ ->
                    error "unreachable: expect qvarid"
            ||]
    ]

rQvarSym :: RuleExpr QualifiedId
rQvarSym = ruleExpr
    [ alt $ tokA @"qvarsym"
        <:> semAct \(qvarsym :* HNil) ->
            [||case $$(qvarsym) of
                TokQualifiedVarSym modid varsym ->
                    QualifiedId (coerce modid) (Id varsym)
                _ ->
                    error "unreachable: expect qvarsym"
            ||]
    ]

rQconId :: RuleExpr QualifiedId
rQconId = ruleExpr
    [ alt $ tokA @"qconid"
        <:> semAct \(qconid :* HNil) ->
            [||case $$(qconid) of
                TokQualifiedConId modid conid ->
                    QualifiedId (coerce modid) (Id conid)
                _ ->
                    error "unreachable: expect qconid"
            ||]
    ]

rQconSym :: RuleExpr QualifiedId
rQconSym = ruleExpr
    [ alt $ tokA @"qconsym"
        <:> semAct \(qconsym :* HNil) ->
            [||case $$(qconsym) of
                TokQualifiedConSym modid consym ->
                    QualifiedId (coerce modid) (Id consym)
                _ ->
                    error "unreachable: expect qconsym"
            ||]
    ]

rModId :: RuleExpr QualifiedId
rModId = ruleExpr
    [ alt $ varA @"qconid"
        <:> semAct \(qconid :* HNil) ->
            qconid
    ]

rExportId :: RuleExpr ()
rExportId = ruleExpr
    [ alt $ varA @"varid"
        <:> semActM \(varid :* HNil) ->
            [||case $$(varid) of
                Id bs | bs == Char8.pack "export" ->
                    pure ()
                _ ->
                    failAction
            ||]
    ]

rHiding :: RuleExpr ()
rHiding = ruleExpr
    [ alt $ varA @"varid"
        <:> semActM \(varid :* HNil) ->
            [||case $$(varid) of
                Id bs | bs == Char8.pack "hiding" ->
                    pure ()
                _ ->
                    failAction
            ||]
    ]

rAs :: RuleExpr ()
rAs = ruleExpr
    [ alt $ varA @"varid"
        <:> semActM \(varid :* HNil) ->
            [||case $$(varid) of
                Id bs | bs == Char8.pack "as" ->
                    pure ()
                _ ->
                    failAction
            ||]
    ]

rQualified :: RuleExpr ()
rQualified = ruleExpr
    [ alt $ varA @"varid"
        <:> semActM \(varid :* HNil) ->
            [||case $$(varid) of
                Id bs | bs == Char8.pack "qualified" ->
                    pure ()
                _ ->
                    failAction
            ||]
    ]

rExclamationSym :: RuleExpr ()
rExclamationSym = ruleExpr
    [ alt $ varA @"varsym"
        <:> semActM \(varsym :* HNil) ->
            [||case $$(varsym) of
                Id bs | bs == Char8.pack "!" ->
                    pure ()
                _ ->
                    failAction
            ||]
    ]

rLiteral :: RuleExpr Lit
rLiteral = ruleExpr
    [ alt $ varA @"integer"
        <:> semAct \(integer :* HNil) ->
            [||LitInteger $$(integer)||]
    , alt $ varA @"float"
        <:> semAct \(float :* HNil) ->
            [||LitFloat $$(float)||]
    , alt $ varA @"string"
        <:> semAct \(string :* HNil) ->
            [||LitString $$(string)||]
    , alt $ varA @"char"
        <:> semAct \(char :* HNil) ->
            [||LitChar $$(char)||]
    ]

rInteger :: RuleExpr Integer
rInteger = ruleExpr
    [ alt $ tokA @"integer"
        <:> semAct \(integer :* HNil) ->
            [||case $$(integer) of
                TokLitInteger bs ->
                    read (Char8.unpack bs) :: Integer
                _ ->
                    error "unreachable: expect integer literal"
            ||]
    ]

rFloat :: RuleExpr Rational
rFloat = ruleExpr
    [ alt $ tokA @"float"
        <:> semAct \(float :* HNil) ->
            [||case $$(float) of
                TokLitFloat bs ->
                    case Numeric.readFloat (Char8.unpack bs) :: [(Rational, String)] of
                        (x, ""):_ ->
                            x
                        [] ->
                            error "unreachable: expect float parser"
                _ ->
                    error "unreachable: expect float literal"
            ||]
    ]

rString :: RuleExpr String
rString = ruleExpr
    [ alt $ tokA @"string"
        <:> semAct \(string :* HNil) ->
            [||case $$(string) of
                TokLitString bs ->
                    read (Char8.unpack bs) :: String
                _ ->
                    error "unreachable: expect string literal"
            ||]
    ]

rChar :: RuleExpr Char
rChar = ruleExpr
    [ alt $ tokA @"char"
        <:> semAct \(char :* HNil) ->
            [||case $$(char) of
                TokLitChar bs ->
                    read (Char8.unpack bs) :: Char
                _ ->
                    error "unreachable: expect char literal"
            ||]
    ]

rExpBo :: RuleExpr ()
rExpBo = ruleExpr
    [ alt $ tokA @"{"
        <:> semActM \(_ :* HNil) ->
            [||modifyAction \l -> 0:l||]
    ]

rExpBc :: RuleExpr ()
rExpBc = ruleExpr
    [ alt $ tokA @"}"
        <:> semActM \(_ :* HNil) ->
            [||do
                l <- getAction
                case l of
                    0:l' ->
                        modifyAction \_ -> l'
                    _ ->
                        failAction
            ||]
    ]

rImpBo :: RuleExpr ()
rImpBo = ruleExpr
    [ alt $ tokA @"{n}"
        <:> semActM \(expB :* HNil) ->
            [||do
                let n = case $$(expB) of
                        TokVirtExpBrace x ->
                            x
                        _ ->
                            error "unreachable: expect virtual brace start point"
                l <- getAction
                case l of
                    m:_
                        | n < m ->
                            modifyAction \l' -> n:l'
                        | otherwise ->
                            modifyAction \l' -> (n + 1):l'
                    []
                        | n > 0 ->
                            modifyAction \l' -> n:l'
                        | otherwise ->
                            failAction
            ||]
    ]

rImpBc :: RuleExpr ()
rImpBc = ruleExpr
    [ eps
        $ semActM \HNil ->
            [||do
                l <- getAction
                case l of
                    m:l' | m > 0 ->
                        modifyAction \_ -> l'
                    _:_ ->
                        failAction
                    [] ->
                        failAction
            ||]
    ]

rSemi :: RuleExpr ()
rSemi = ruleExpr
    [ alt $ tokA @";"
        <:> semAct \(_ :* HNil) ->
            [||()||]
    , alt $ tokA @"<n>"
        <:> semActM \(nl :* HNil) ->
            [||do
                let n = case $$(nl) of
                        TokVirtNewline x ->
                            x
                        _ ->
                            error "unreachable: expect newline"
                l <- getAction
                case l of
                    m:_ | n == m ->
                        pure ()
                    _:_ ->
                        failAction
                    [] ->
                        failAction
            ||]
    ]
