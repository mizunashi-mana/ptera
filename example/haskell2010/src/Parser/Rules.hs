{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Parser.Rules where

import           Data.Coerce
import           Data.Foldable
import           Data.Proxy                       (Proxy (..))
import           Data.Sequence                    (Seq)
import qualified Data.Sequence                    as Seq
import qualified Data.Text                        as Text
import           GHC.TypeLits                     (KnownSymbol, Symbol)
import qualified Language.Haskell.TH              as TH
import           Language.Parser.Ptera.Data.HEnum (henumA)
import           Language.Parser.Ptera.Data.HList (HList (..))
import           Language.Parser.Ptera.TH         (TokensTag, eps, failAction,
                                                   getAction, modifyAction,
                                                   ruleExpr, semAct, semActM,
                                                   varA, (<:>), (<^>))
import qualified Language.Parser.Ptera.TH         as Ptera
import qualified Numeric
import qualified Type.Membership                  as Membership
import qualified Type.Membership.Internal         as MembershipInternal
import           Types


$(Ptera.genGrammarToken (TH.mkName "Tokens") [t|Token|]
    [ ("(",         [p|TokSpOpenParen{}|])
    , (")",         [p|TokSpCloseParen{}|])
    , (",",         [p|TokSpComma{}|])
    , (";",         [p|TokSpSemicolon{}|])
    , ("[",         [p|TokSpOpenBracket{}|])
    , ("]",         [p|TokSpCloseBracket{}|])
    , ("`",         [p|TokSpBacktick{}|])
    , ("{",         [p|TokSpOpenBrace{}|])
    , ("}",         [p|TokSpCloseBrace{}|])

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

    , ("{n}",       [p|TokVirtExpBrace{}|])
    , ("<n>",       [p|TokVirtNewline{}|])
    , ("EOS",       [p|TokVirtEndOfInput{}|])
    ])

type GrammarContext = [Int]

$(Ptera.genRules
    do TH.mkName "Rules"
    do Ptera.GenRulesTypes
        { Ptera.genRulesCtxTy = [t|GrammarContext|]
        , Ptera.genRulesTokensTy = [t|Tokens|]
        , Ptera.genRulesTokenTy = [t|Token|]
        }
    [ (TH.mkName "rmoduleeos", "module EOS", [t|Program|])

    , (TH.mkName "rmodule", "module", [t|Program|])
    , (TH.mkName "rbody", "body", [t|ProgramBody|])
    , (TH.mkName "rbodyinl", "bodyinl", [t|ProgramBody|])
    , (TH.mkName "rimpdecls", "impdecls", [t|Seq ImportDecl|])
    , (TH.mkName "rimpdecls1", "(impdecl semi+)* impdecl", [t|Seq ImportDecl|])
    , (TH.mkName "rexports", "exports", [t|[ExportItem]|])
    , (TH.mkName "rexports0", "(export ',')* export?", [t|Seq ExportItem|])
    , (TH.mkName "rexport", "export", [t|ExportItem|])
    , (TH.mkName "rcnames", "((cname ',')* cname)?", [t|[Id]|])
    , (TH.mkName "rcnames1", "(cname ',')* cname", [t|Seq Id|])
    , (TH.mkName "rimpdecl", "impdecl", [t|Seq ImportDecl|])
    , (TH.mkName "rasmodidopt", "('as' modid)?", [t|Maybe QualifiedId|])
    , (TH.mkName "rimpspecopt", "impspec?", [t|Maybe ImportSpec|])
    , (TH.mkName "rimpspec", "impspec", [t|ImportSpec|])
    , (TH.mkName "rimports", "(import ',')* import?", [t|Seq ImportItem|])
    , (TH.mkName "rimport", "import", [t|ImportItem|])
    , (TH.mkName "rcname", "cname", [t|Id|])

    , (TH.mkName "rtopdecls", "topdecls", [t|[Decl]|])
    , (TH.mkName "rtopdecls1", "(topdecl semi)* topdecl", [t|Seq Decl|])
    , (TH.mkName "rtopdecl", "topdecl", [t|Seq Decl|])
    , (TH.mkName "rderivingopt", "deriving?", [t|Maybe Deriving|])
    , (TH.mkName "rcontextopt", "(context '=>')?", [t|Maybe Context|])
    , (TH.mkName "rscontextopt", "(scontext '=>')?", [t|Maybe Context|])
    , (TH.mkName "rwherecdeclsopt", "('where' cdecls)?", [t|[Decl]|])
    , (TH.mkName "rwhereideclsopt", "('where' idecls)?", [t|[Decl]|])
    , (TH.mkName "rtypes", "((type ',')* type)?", [t|[Type]|])
    , (TH.mkName "rtypes1", "(type ',')* type", [t|Seq Type|])

    , (TH.mkName "rdecls", "decls", [t|[Decl]|])
    , (TH.mkName "rdeclsinl", "declsinl", [t|[Decl]|])
    , (TH.mkName "rdeclsinl1", "(decl semi)* decl", [t|Seq Decl|])
    , (TH.mkName "rdecl", "decl", [t|Seq Decl|])
    , (TH.mkName "rcdecls", "cdecls", [t|[Decl]|])
    , (TH.mkName "rcdeclsinl", "cdeclsinl", [t|[Decl]|])
    , (TH.mkName "rcdeclsinl1", "(cdecl semi)* cdecl", [t|Seq Decl|])
    , (TH.mkName "rcdecl", "cdecl", [t|Seq Decl|])
    , (TH.mkName "ridecls", "idecls", [t|[Decl]|])
    , (TH.mkName "rideclsinl", "ideclsinl", [t|[Decl]|])
    , (TH.mkName "rideclsinl1", "(idecl semi)* idecl", [t|Seq Decl|])
    , (TH.mkName "ridecl", "idecl", [t|Seq Decl|])
    , (TH.mkName "rgendecl", "gendecl", [t|Seq Decl|])
    , (TH.mkName "rops", "ops", [t|Seq Id|])
    , (TH.mkName "rvars", "vars", [t|Seq Id|])
    , (TH.mkName "rfixity", "fixity", [t|Fixity|])

    , (TH.mkName "rtype", "type", [t|Type|])
    , (TH.mkName "rbtype", "btype", [t|Type|])
    , (TH.mkName "ratypes", "atype*", [t|Seq Type|])
    , (TH.mkName "ratype", "atype", [t|Type|])
    , (TH.mkName "rtypes2", "(type ',')+ type", [t|Seq Type|])
    , (TH.mkName "rgtycon", "gtycon", [t|Type|])
    , (TH.mkName "rcommas1", "','+", [t|Int|])

    , (TH.mkName "rcontext", "context", [t|Context|])
    , (TH.mkName "rclasses", "((class ',')* class)?", [t|[Type]|])
    , (TH.mkName "rclasses1", "(class ',')* class", [t|Seq Type|])
    , (TH.mkName "rclass", "class", [t|Type|])
    , (TH.mkName "ratypes1", "atype+", [t|Seq Type|])
    , (TH.mkName "rscontext", "scontext", [t|Context|])
    , (TH.mkName "rsimpleclasses", "((simpleclass ',')* simpleclass)?", [t|[Type]|])
    , (TH.mkName "rsimpleclasses1", "(simpleclass ',')* simpleclass", [t|Seq Type|])
    , (TH.mkName "rsimpleclass", "simpleclass", [t|Type|])

    , (TH.mkName "rsimpletype", "simpletype", [t|Type|])
    , (TH.mkName "rtyvars", "tyvar*", [t|Seq Type|])
    , (TH.mkName "rconstrs", "constrs", [t|Seq Constr|])
    , (TH.mkName "rconstr", "constr", [t|Constr|])
    , (TH.mkName "rfielddecls", "((fielddecl ',')* fielddecl)?", [t|[(Strictness, [Id], Type)]|])
    , (TH.mkName "rfielddecls1", "(fielddecl ',')* fielddecl", [t|Seq (Strictness, [Id], Type)|])
    , (TH.mkName "rabctype", "btype | '!' atype", [t|(Strictness, Type)|])
    , (TH.mkName "ractypes", "('!'? atype)*", [t|Seq (Strictness, Type)|])
    , (TH.mkName "ractype", "'!'? atype", [t|(Strictness, Type)|])
    , (TH.mkName "rnewconstr", "newconstr", [t|Constr|])
    , (TH.mkName "rfielddecl", "fielddecl", [t|(Strictness, [Id], Type)|])
    , (TH.mkName "rderiving", "deriving", [t|Deriving|])
    , (TH.mkName "rdclasses", "((dclass ',')* dclass)?", [t|[Type]|])
    , (TH.mkName "rdclasses1", "(dclass ',')* dclass", [t|Seq Type|])
    , (TH.mkName "rdclass", "dclass", [t|Type|])
    , (TH.mkName "rinst", "inst", [t|Type|])
    , (TH.mkName "rtyvars2", "(tyvar ',')+ tyvar", [t|Seq Type|])

    , (TH.mkName "rfdecl", "fdecl", [t|Decl|])
    , (TH.mkName "rcallconv", "callconv", [t|ForeignCallConv|])
    , (TH.mkName "rsafetyopt", "safety?", [t|Maybe Safety|])
    , (TH.mkName "rimpent", "impent", [t|Maybe String|])
    , (TH.mkName "rexpent", "expent", [t|Maybe String|])
    , (TH.mkName "rsafety", "safety", [t|Safety|])
    , (TH.mkName "rftype", "ftype", [t|Type|])
    , (TH.mkName "rfrtype", "frtype", [t|Type|])
    , (TH.mkName "rfatype", "fatype", [t|Type|])

    , (TH.mkName "rfunlhs", "funlhs", [t|(Id, Seq Pat)|])
    , (TH.mkName "rapats1", "apat+", [t|Seq Pat|])
    , (TH.mkName "rrhs", "rhs", [t|Rhs|])
    , (TH.mkName "rwheredeclsopt", "('where' decls)?", [t|[Decl]|])
    , (TH.mkName "rgdrhs", "gdrhs", [t|Seq ([Guard], Exp)|])
    , (TH.mkName "rguards", "guards", [t|[Guard]|])
    , (TH.mkName "rguards1", "(guard ',')* guard", [t|Seq Guard|])
    , (TH.mkName "rguard", "guard", [t|Guard|])

    , (TH.mkName "rexp", "exp", [t|Exp|])
    , (TH.mkName "rinfixexp", "infixexp", [t|Exp|])
    , (TH.mkName "rlexp", "lexp", [t|Exp|])
    , (TH.mkName "rsemiopt", "semi?", [t|()|])
    , (TH.mkName "rfexp", "fexp", [t|Exp|])
    , (TH.mkName "raexps", "aexp*", [t|Seq Exp|])
    , (TH.mkName "raexp", "aexp", [t|Exp|])
    , (TH.mkName "rrecupdates", "(expbo ((fbind ',')* fbind)? expbc)*", [t|Seq [(QualifiedId, Exp)]|])
    , (TH.mkName "raexp2", "aexp2", [t|Exp|])
    , (TH.mkName "rexps2", "(exp ',')+ exp", [t|Seq Exp|])
    , (TH.mkName "rexps1", "(exp ',')* exp", [t|Seq Exp|])
    , (TH.mkName "rcexpopt", "(',' exp)?", [t|Maybe Exp|])
    , (TH.mkName "rexpopt", "exp?", [t|Maybe Exp|])
    , (TH.mkName "rquals1", "(qual ',')* qual", [t|Seq Guard|])
    , (TH.mkName "rfbinds", "((fbind ',')* fbind)?", [t|[(QualifiedId, Exp)]|])
    , (TH.mkName "rfbinds1", "(fbind ',')* fbind", [t|Seq (QualifiedId, Exp)|])

    , (TH.mkName "rqual", "qual", [t|Guard|])
    , (TH.mkName "rcasealts", "casealts", [t|[CaseAlt]|])
    , (TH.mkName "ralts", "alts", [t|Seq CaseAlt|])
    , (TH.mkName "ralt", "alt", [t|Seq CaseAlt|])
    , (TH.mkName "rgdpat", "gdpat", [t|Seq ([Guard], Exp)|])
    , (TH.mkName "rdostmts", "dostmts", [t|([Stmt], Exp)|])
    , (TH.mkName "rstmts", "stmts", [t|([Stmt], Exp)|])
    , (TH.mkName "rstmts0", "stmt*", [t|Seq Stmt|])
    , (TH.mkName "rstmt", "stmt", [t|Seq Stmt|])
    , (TH.mkName "rfbind", "fbind", [t|(QualifiedId, Exp)|])

    , (TH.mkName "rpat", "pat", [t|Pat|])
    , (TH.mkName "rlpat", "lpat", [t|Pat|])
    , (TH.mkName "rapat", "apat", [t|Pat|])
    , (TH.mkName "rpats2", "(pat ',')+ pat", [t|Seq Pat|])
    , (TH.mkName "rpats1", "(pat ',')* pat", [t|Seq Pat|])
    , (TH.mkName "rfpats", "((fpat ',')* fpat)?", [t|[(QualifiedId, Pat)]|])
    , (TH.mkName "rfpats1", "(fpat ',')* fpat", [t|Seq (QualifiedId, Pat)|])
    , (TH.mkName "rfpat", "fpat", [t|(QualifiedId, Pat)|])

    , (TH.mkName "rgcon", "gcon", [t|Gcon|])
    , (TH.mkName "rvar", "var", [t|Id|])
    , (TH.mkName "rqvar", "qvar", [t|QualifiedId|])
    , (TH.mkName "rcon", "con", [t|Id|])
    , (TH.mkName "rqcon", "qcon", [t|QualifiedId|])
    , (TH.mkName "rvarop", "varop", [t|Id|])
    , (TH.mkName "rqvarop", "qvarop", [t|QualifiedId|])
    , (TH.mkName "rconop", "conop", [t|Id|])
    , (TH.mkName "rqconop", "qconop", [t|QualifiedId|])
    , (TH.mkName "rop", "op", [t|Id|])
    , (TH.mkName "rqop", "qop", [t|QualifiedId|])
    , (TH.mkName "rgconsym", "gconsym", [t|QualifiedId|])
    , (TH.mkName "rtyvar", "tyvar", [t|Id|])
    , (TH.mkName "rtycon", "tycon", [t|Id|])
    , (TH.mkName "rqtycon", "qtycon", [t|QualifiedId|])
    , (TH.mkName "rvarid", "varid", [t|Id|])
    , (TH.mkName "rvarsym", "varsym", [t|Id|])
    , (TH.mkName "rconid", "conid", [t|Id|])
    , (TH.mkName "rconsym", "consym", [t|Id|])
    , (TH.mkName "rqvarid", "qvarid", [t|QualifiedId|])
    , (TH.mkName "rqvarsym", "qvarsym", [t|QualifiedId|])
    , (TH.mkName "rqconid", "qconid", [t|QualifiedId|])
    , (TH.mkName "rqconsym", "qconsym", [t|QualifiedId|])
    , (TH.mkName "rmodid", "modid", [t|QualifiedId|])
    , (TH.mkName "ridexport", "'export'", [t|()|])
    , (TH.mkName "ridhiding", "'hiding'", [t|()|])
    , (TH.mkName "ridas", "'as'", [t|()|])
    , (TH.mkName "ridqualified", "'qualified'", [t|()|])
    , (TH.mkName "rsymexclamation", "'!'", [t|()|])

    , (TH.mkName "rliteral", "literal", [t|Lit|])
    , (TH.mkName "rinteger", "integer", [t|Integer|])
    , (TH.mkName "rfloat", "float", [t|Rational|])
    , (TH.mkName "rstring", "string", [t|String|])
    , (TH.mkName "rchar", "char", [t|Char|])

    , (TH.mkName "rexpbo", "expbo", [t|()|])
    , (TH.mkName "rexpbc", "expbc", [t|()|])
    , (TH.mkName "rimpbo", "impbo", [t|()|])
    , (TH.mkName "rimpbc", "impbc", [t|()|])
    , (TH.mkName "rsemi", "semi", [t|()|])
    , (TH.mkName "rsemis0", "semi*", [t|()|])
    , (TH.mkName "rsemis1", "semi+", [t|()|])

    , (TH.mkName "rskip", "skip", [t|()|])
    ])

grammar :: Ptera.GrammarM GrammarContext Rules Tokens Token ParsePoints
grammar = Ptera.fixGrammar $ Rules
    { rmoduleeos = rModuleEos

    , rmodule = rModule
    , rbody = rBody
    , rbodyinl = rBodyInL
    , rimpdecls = rImpDecls
    , rimpdecls1 = rImpDecls1
    , rexports = rExports
    , rexports0 = rExports0
    , rexport = rExport
    , rcnames = rCnames
    , rcnames1 = rCnames1
    , rimpdecl = rImpDecl
    , rasmodidopt = rAsModIdOpt
    , rimpspecopt = rImpSpecOpt
    , rimpspec = rImpSpec
    , rimports = rImports
    , rimport = rImport
    , rcname = rCname
    , rtopdecls = rTopDecls
    , rtopdecls1 = rTopDecls1
    , rtopdecl = rTopDecl
    , rderivingopt = rDerivingOpt
    , rcontextopt = rContextOpt
    , rscontextopt = rScontextOpt
    , rwherecdeclsopt = rWhereCdeclsOpt
    , rwhereideclsopt = rWhereIdeclsOpt
    , rtypes = rTypes
    , rtypes1 = rTypes1

    , rdecls = rDecls
    , rdeclsinl = rDeclsInL
    , rdeclsinl1 = rDeclsInL1
    , rdecl = rDecl
    , rcdecls = rCdecls
    , rcdeclsinl = rCdeclsInL
    , rcdeclsinl1 = rCdeclsInL1
    , rcdecl = rCdecl
    , ridecls = rIdecls
    , rideclsinl = rIdeclsInL
    , rideclsinl1 = rIdeclsInL1
    , ridecl = rIdecl
    , rgendecl = rGenDecl
    , rops = rOps
    , rvars = rVars
    , rfixity = rFixity

    , rtype = rType
    , rbtype = rBtype
    , ratypes = rAtypes
    , ratype = rAtype
    , rtypes2 = rTypes2
    , rgtycon = rGtycon
    , rcommas1 = rCommas1

    , rcontext = rContext
    , rclasses = rClasses
    , rclasses1 = rClasses1
    , rclass = rClass
    , ratypes1 = rAtypes1
    , rscontext = rScontext
    , rsimpleclasses = rSimpleClasses
    , rsimpleclasses1 = rSimpleClasses1
    , rsimpleclass = rSimpleClass

    , rsimpletype = rSimpleType
    , rtyvars = rTyVars
    , rconstrs = rConstrs
    , rconstr = rConstr
    , rfielddecls = rFieldDecls
    , rfielddecls1 = rFieldDecls1
    , rabctype = rAbctype
    , ractypes = rActypes
    , ractype = rActype
    , rnewconstr = rNewConstr
    , rfielddecl = rFieldDecl
    , rderiving = rDeriving
    , rdclasses = rDclasses
    , rdclasses1 = rDclasses1
    , rdclass = rDclass
    , rinst = rInst
    , rtyvars2 = rTyVars2

    , rfdecl = rFdecl
    , rcallconv = rCallConv
    , rsafetyopt = rSafetyOpt
    , rimpent = rImpent
    , rexpent = rExpent
    , rsafety = rSafety
    , rftype = rFtype
    , rfrtype = rFrtype
    , rfatype = rFatype

    , rfunlhs = rFunlhs
    , rapats1 = rApats1
    , rrhs = rRhs
    , rwheredeclsopt = rWhereDeclsOpt
    , rgdrhs = rGdrhs
    , rguards = rGuards
    , rguards1 = rGuards1
    , rguard = rGuard

    , rexp = rExp
    , rinfixexp = rInfixExp
    , rlexp = rLexp
    , rsemiopt = rSemiOpt
    , rfexp = rFexp
    , raexps = rAexps
    , raexp = rAexp
    , rrecupdates = rRecUpdates
    , raexp2 = rAexp2
    , rexps2 = rExps2
    , rexps1 = rExps1
    , rcexpopt = rCexpOpt
    , rexpopt = rExpOpt
    , rquals1 = rQuals1
    , rfbinds = rFbinds
    , rfbinds1 = rFbinds1

    , rqual = rQual
    , rcasealts = rCaseAlts
    , ralts = rAlts
    , ralt = rAlt
    , rgdpat = rGdpat
    , rdostmts = rDoStmts
    , rstmts = rStmts
    , rstmts0 = rStmts0
    , rstmt = rStmt
    , rfbind = rFbind

    , rpat = rPat
    , rlpat = rLpat
    , rapat = rApat
    , rpats2 = rPats2
    , rpats1 = rPats1
    , rfpats = rFpats
    , rfpats1 = rFpats1
    , rfpat = rFpat

    , rgcon = rGcon
    , rvar = rVar
    , rqvar = rQvar
    , rcon = rCon
    , rqcon = rQcon
    , rvarop = rVarOp
    , rqvarop = rQvarOp
    , rconop = rConOp
    , rqconop = rQconOp
    , rop = rOp
    , rqop = rQop
    , rgconsym = rGconsym
    , rtyvar = rTyvar
    , rtycon = rTycon
    , rqtycon = rQtycon
    , rvarid = rVarId
    , rvarsym = rVarSym
    , rconid = rConId
    , rconsym = rConSym
    , rqvarid = rQvarId
    , rqvarsym = rQvarSym
    , rqconid = rQconId
    , rqconsym = rQconSym
    , rmodid = rModId
    , ridexport = rIdExport
    , ridhiding = rIdHiding
    , ridas = rIdAs
    , ridqualified = rIdQualified
    , rsymexclamation = rSymExclamation

    , rliteral = rLiteral
    , rinteger = rInteger
    , rfloat = rFloat
    , rstring = rString
    , rchar = rChar

    , rexpbo = rExpBo
    , rexpbc = rExpBc
    , rimpbo = rImpBo
    , rimpbc = rImpBc
    , rsemi = rSemi
    , rsemis0 = rSemis0
    , rsemis1 = rSemis1

    , rskip = rSkip
    }

type ParsePoints = '["module EOS", "gtycon"]

type RuleExpr = Ptera.RuleExprM GrammarContext Rules Tokens Token
type Alt = Ptera.AltM GrammarContext Rules Tokens Token
type SemAct = Ptera.SemActM GrammarContext

rModuleEos :: RuleExpr Program
rModuleEos = ruleExpr
    [ varA @"module" <^> tokA @"EOS"
        <:> semAct \(mod :* _ :* _ :* HNil) ->
            mod
    ]

rModule :: RuleExpr Program
rModule = ruleExpr
    [ tokA @"module" <^> varA @"modid" <^> varA @"exports" <^> tokA @"where" <^> varA @"body"
        <:> semAct \(_ :* _ :* modId :* exports :* _ :* _ :* body :* HNil) ->
            [||Program (Just $$(modId)) $$(exports) $$(body)||]
    , tokA @"module" <^> varA @"modid" <^> tokA @"where" <^> varA @"body"
        <:> semAct \(_ :* _ :* modId :* _ :* _ :* body :* HNil) ->
            [||Program (Just $$(modId)) [] $$(body)||]
    , varA @"body"
        <:> semAct \(body :* HNil) ->
            [||Program Nothing [] $$(body)||]
    ]

rBody :: RuleExpr ProgramBody
rBody = ruleExpr
    [ varA @"expbo" <^> varA @"bodyinl" <^> varA @"expbc"
        <:> semAct \(_ :* body :* _ :* HNil) -> body
    , varA @"impbo" <^> varA @"bodyinl" <^> varA @"impbc"
        <:> semAct \(_ :* body :* _ :* HNil) -> body
    ]

rBodyInL :: RuleExpr ProgramBody
rBodyInL = ruleExpr
    [ varA @"impdecls" <^> varA @"semi" <^> varA @"topdecls"
        <:> semAct \(impdecls :* _ :* topdecls :* HNil) ->
            [||ProgramBody (seqToList $$(impdecls)) $$(topdecls)||]
    , varA @"impdecls"
        <:> semAct \(impdecls :* HNil) ->
            [||ProgramBody (seqToList $$(impdecls)) []||]
    , varA @"topdecls"
        <:> semAct \(topdecls :* HNil) ->
            [||ProgramBody [] $$(topdecls)||]
    ]

rImpDecls :: RuleExpr (Seq ImportDecl)
rImpDecls = ruleExpr
    [ varA @"semi*" <^> varA @"(impdecl semi+)* impdecl"
        <:> semAct \(_ :* impdecls :* HNil) ->
            impdecls
    ]

rImpDecls1 :: RuleExpr (Seq ImportDecl)
rImpDecls1 = ruleExpr
    [ varA @"impdecl" <^> varA @"semi+" <^> varA @"(impdecl semi+)* impdecl"
        <:> semAct \(impdecl :* _ :* impdecls :* HNil) ->
            [||$$(impdecl) Seq.>< $$(impdecls)||]
    , varA @"impdecl"
        <:> semAct \(impdecl :* HNil) ->
            impdecl
    ]

rExports :: RuleExpr [ExportItem]
rExports = ruleExpr
    [ tokA @"(" <^> varA @"(export ',')* export?" <^> tokA @")"
        <:> semAct \(_ :* _ :* exports :* _ :* _ :* HNil) ->
            [||seqToList $$(exports)||]
    ]

rExports0 :: RuleExpr (Seq ExportItem)
rExports0 = ruleExpr
    [ varA @"export" <^> tokA @"," <^> varA @"(export ',')* export?"
        <:> semAct \(export :* _ :* _ :* exports :* HNil) ->
            [||$$(export) Seq.:<| $$(exports)||]
    , varA @"export"
        <:> semAct \(export :* HNil) ->
            [||Seq.singleton $$(export)||]
    , eps
        $ semAct \HNil ->
            [||Seq.empty||]
    ]

rExport :: RuleExpr ExportItem
rExport = ruleExpr
    [ varA @"qvar"
        <:> semAct \(qvar :* HNil) ->
            [||ExportItemId $$(qvar)||]
    , varA @"qtycon" <^> tokA @"(" <^> tokA @".." <^> tokA @")"
        <:> semAct \(qtycon :* _ :* _ :* _ :* _ :* _ :* _ :* HNil) ->
            [||ExportItemTyConAll $$(qtycon)||]
    , varA @"qtycon" <^> tokA @"(" <^> varA @"((cname ',')* cname)?" <^> tokA @")"
        <:> semAct \(qtycon :* _ :* _ :* cnames :* _ :* _ :* HNil) ->
            [||ExportItemTyConSpecified $$(qtycon) $$(cnames)||]
    , varA @"qtycon"
        <:> semAct \(qtycon :* HNil) ->
            [||ExportItemTyConSpecified $$(qtycon) []||]
    , tokA @"module" <^> varA @"modid"
        <:> semAct \(_ :* _ :* modid :* HNil) ->
            [||ExportItemModule $$(modid)||]
    ]

rCnames :: RuleExpr [Id]
rCnames = ruleExpr
    [ varA @"(cname ',')* cname"
        <:> semAct \(cnames1 :* HNil) ->
            [||seqToList $$(cnames1)||]
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rCnames1 :: RuleExpr (Seq Id)
rCnames1 = ruleExpr
    [ varA @"cname" <^> tokA @"," <^> varA @"(cname ',')* cname"
        <:> semAct \(cname :* _ :* _ :* cnames1 :* HNil) ->
            [||$$(cname) Seq.:<| $$(cnames1)||]
    , varA @"cname"
        <:> semAct \(cname :* HNil) ->
            [||Seq.singleton $$(cname)||]
    ]

rImpDecl :: RuleExpr (Seq ImportDecl)
rImpDecl = ruleExpr
    [ tokA @"import" <^> varA @"'qualified'" <^> varA @"modid" <^> varA @"('as' modid)?" <^> varA @"impspec?"
        <:> semAct \(_ :* _ :* _ :* modid :* asmodidOpt :* impspecOpt :* HNil) ->
            [||Seq.singleton $ ImportDecl True $$(modid) $$(asmodidOpt) $$(impspecOpt)||]
    , tokA @"import" <^> varA @"modid" <^> varA @"('as' modid)?" <^> varA @"impspec?"
        <:> semAct \(_ :* _ :* modid :* asmodidOpt :* impspecOpt :* HNil) ->
            [||Seq.singleton $ ImportDecl False $$(modid) $$(asmodidOpt) $$(impspecOpt)||]
    ]

rAsModIdOpt :: RuleExpr (Maybe QualifiedId)
rAsModIdOpt = ruleExpr
    [ varA @"'as'" <^> varA @"modid"
        <:> semAct \(_ :* modid :* HNil) ->
            [||Just $$(modid)||]
    , eps
        $ semAct \HNil ->
            [||Nothing||]
    ]

rImpSpecOpt :: RuleExpr (Maybe ImportSpec)
rImpSpecOpt = ruleExpr
    [ varA @"impspec"
        <:> semAct \(impspec :* HNil) ->
            [||Just $$(impspec)||]
    , eps
        $ semAct \HNil ->
            [||Nothing||]
    ]

rImpSpec :: RuleExpr ImportSpec
rImpSpec = ruleExpr
    [ tokA @"(" <^> varA @"(import ',')* import?" <^> tokA @")"
        <:> semAct \(_ :* _ :* imports :* _ :* _ :* HNil) ->
            [||ImportSpecSpecified (seqToList $$(imports))||]
    , varA @"'hiding'" <^> tokA @"(" <^> varA @"(import ',')* import?" <^> tokA @")"
        <:> semAct \(_ :* _ :* _ :* imports :* _ :* _ :* HNil) ->
            [||ImportSpecHiding (seqToList $$(imports))||]
    ]

rImports :: RuleExpr (Seq ImportItem)
rImports = ruleExpr
    [ varA @"import" <^> tokA @"," <^> varA @"(import ',')* import?"
        <:> semAct \(imp :* _ :* _ :* imports :* HNil) ->
            [||$$(imp) Seq.:<| $$(imports)||]
    , varA @"import"
        <:> semAct \(imp :* HNil) ->
            [||Seq.singleton $$(imp)||]
    , eps
        $ semAct \HNil ->
            [||Seq.empty||]
    ]

rImport :: RuleExpr ImportItem
rImport = ruleExpr
    [ varA @"var"
        <:> semAct \(var :* HNil) ->
            [||ImportItemId $$(var)||]
    , varA @"tycon" <^> tokA @"(" <^> tokA @".." <^> tokA @")"
        <:> semAct \(tycon :* _ :* _ :* _ :* _ :* _ :* _ :* HNil) ->
            [||ImportItemTyConAll $$(tycon)||]
    , varA @"tycon" <^> tokA @"(" <^> varA @"((cname ',')* cname)?" <^> tokA @")"
        <:> semAct \(tycon :* _ :* _ :* cnames :* _ :* _ :* HNil) ->
            [||ImportItemTyConSpecified $$(tycon) $$(cnames)||]
    ]

rCname :: RuleExpr Id
rCname = ruleExpr
    [ varA @"var"
        <:> semAct \(var :* HNil) ->
            var
    , varA @"con"
        <:> semAct \(con :* HNil) ->
            con
    ]

rTopDecls :: RuleExpr [Decl]
rTopDecls = ruleExpr
    [ varA @"(topdecl semi)* topdecl"
        <:> semAct \(topdecls1 :* HNil) ->
            [||seqToList $$(topdecls1)||]
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rTopDecls1 :: RuleExpr (Seq Decl)
rTopDecls1 = ruleExpr
    [ varA @"topdecl" <^> varA @"semi" <^> varA @"(topdecl semi)* topdecl"
        <:> semAct \(topdecl :* _ :* topdecls1 :* HNil) ->
            [||$$(topdecl) Seq.>< $$(topdecls1)||]
    , varA @"topdecl"
        <:> semAct \(topdecl :* HNil) ->
            topdecl
    ]

rTopDecl :: RuleExpr (Seq Decl)
rTopDecl = ruleExpr
    [ tokA @"type" <^> varA @"simpletype" <^> tokA @"=" <^> varA @"type"
        <:> semAct \(_ :* _ :* simpletype :* _ :* _ :* ty :* HNil) ->
            [||Seq.singleton $ DeclType $$(simpletype) $$(ty)||]
    , tokA @"data" <^> varA @"(context '=>')?" <^> varA @"simpletype" <^> tokA @"=" <^> varA @"constrs" <^> varA @"deriving?"
        <:> semAct \(_ :* _ :* contextopt :* simpletype :* _ :* _ :* constrs :* derivingopt :* HNil) ->
            [||Seq.singleton $ DeclData $$(contextopt) $$(simpletype) (seqToList $$(constrs)) $$(derivingopt)||]
    , tokA @"data" <^> varA @"(context '=>')?" <^> varA @"simpletype" <^> varA @"deriving?"
        <:> semAct \(_ :* _ :* contextopt :* simpletype :* derivingopt :* HNil) ->
            [||Seq.singleton $ DeclData $$(contextopt) $$(simpletype) [] $$(derivingopt)||]
    , tokA @"newtype" <^> varA @"(context '=>')?" <^> varA @"simpletype" <^> tokA @"=" <^> varA @"newconstr" <^> varA @"deriving?"
        <:> semAct \(_ :* _ :* contextopt :* simpletype :* _ :* _ :* newconstr :* derivingopt :* HNil) ->
            [||Seq.singleton $ DeclNewtype $$(contextopt) $$(simpletype) $$(newconstr) $$(derivingopt)||]
    , tokA @"class" <^> varA @"(scontext '=>')?" <^> varA @"tycon" <^> varA @"tyvar" <^> varA @"('where' cdecls)?"
        <:> semAct \(_ :* _ :* contextopt :* tycon :* tyvar :* cdecls :* HNil) ->
            [||Seq.singleton $ DeclClass $$(contextopt) $$(tycon) $$(tyvar) $$(cdecls)||]
    , tokA @"instance" <^> varA @"(scontext '=>')?" <^> varA @"qtycon" <^> varA @"inst" <^> varA @"('where' idecls)?"
        <:> semAct \(_ :* _ :* contextopt :* qtycon :* inst :* idecls :* HNil) ->
            [||Seq.singleton $ DeclInstance $$(contextopt) $$(qtycon) $$(inst) $$(idecls)||]
    , tokA @"default" <^> tokA @"(" <^> varA @"((type ',')* type)?" <^> tokA @")"
        <:> semAct \(_ :* _ :* _ :* _ :* types :* _ :* _ :* HNil) ->
            [||Seq.singleton $ DeclDefault $$(types)||]
    , tokA @"foreign" <^> varA @"fdecl"
        <:> semAct \(_ :* _ :* fdecl :* HNil) ->
            [||Seq.singleton $$(fdecl)||]
    , varA @"decl"
        <:> semAct \(decl :* HNil) ->
            decl
    ]

rDerivingOpt :: RuleExpr (Maybe Deriving)
rDerivingOpt = ruleExpr
    [ varA @"deriving"
        <:> semAct \(deriv :* HNil) ->
            [||Just $$(deriv)||]
    , eps
        $ semAct \HNil ->
            [||Nothing||]
    ]

rContextOpt :: RuleExpr (Maybe Context)
rContextOpt = ruleExpr
    [ varA @"context" <^> tokA @"=>"
        <:> semAct \(context :* _ :* _ :* HNil) ->
            [||Just $$(context)||]
    , eps
        $ semAct \HNil ->
            [||Nothing||]
    ]

rScontextOpt :: RuleExpr (Maybe Context)
rScontextOpt = ruleExpr
    [ varA @"scontext" <^> tokA @"=>"
        <:> semAct \(context :* _ :* _ :* HNil) ->
            [||Just $$(context)||]
    , eps
        $ semAct \HNil ->
            [||Nothing||]
    ]

rWhereCdeclsOpt :: RuleExpr [Decl]
rWhereCdeclsOpt = ruleExpr
    [ tokA @"where" <^> varA @"cdecls"
        <:> semAct \(_ :* _ :* cdecls :* HNil) ->
            cdecls
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rWhereIdeclsOpt :: RuleExpr [Decl]
rWhereIdeclsOpt = ruleExpr
    [ tokA @"where" <^> varA @"idecls"
        <:> semAct \(_ :* _ :* idecls :* HNil) ->
            idecls
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rTypes :: RuleExpr [Type]
rTypes = ruleExpr
    [ varA @"(type ',')* type"
        <:> semAct \(types1 :* HNil) ->
            [||seqToList $$(types1)||]
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rTypes1 :: RuleExpr (Seq Type)
rTypes1 = ruleExpr
    [ varA @"type" <^> tokA @"," <^> varA @"(type ',')* type"
        <:> semAct \(ty :* _ :* _ :* types1 :* HNil) ->
            [||$$(ty) Seq.:<| $$(types1)||]
    , varA @"type"
        <:> semAct \(ty :* HNil) ->
            [||Seq.singleton $$(ty)||]
    ]

rDecls :: RuleExpr [Decl]
rDecls = ruleExpr
    [ varA @"expbo" <^> varA @"declsinl" <^> varA @"expbc"
        <:> semAct \(_ :* declsinl :* _ :* HNil) ->
            declsinl
    , varA @"impbo" <^> varA @"declsinl" <^> varA @"impbc"
        <:> semAct \(_ :* declsinl :* _ :* HNil) ->
            declsinl
    ]

rDeclsInL :: RuleExpr [Decl]
rDeclsInL = ruleExpr
    [ varA @"(decl semi)* decl"
        <:> semAct \(declsinl1 :* HNil) ->
            [||seqToList $$(declsinl1)||]
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rDeclsInL1 :: RuleExpr (Seq Decl)
rDeclsInL1 = ruleExpr
    [ varA @"decl" <^> varA @"semi" <^> varA @"(decl semi)* decl"
        <:> semAct \(decl :* _ :* declsinl1 :* HNil) ->
            [||$$(decl) Seq.>< $$(declsinl1)||]
    , varA @"decl"
        <:> semAct \(decl :* HNil) ->
            decl
    ]

rDecl :: RuleExpr (Seq Decl)
rDecl = ruleExpr
    [ varA @"funlhs" <^> varA @"rhs"
        <:> semAct \(funlhs :* rhs :* HNil) ->
            [||case $$(funlhs) of
                (f, args) ->
                    Seq.singleton $ DeclFun f (seqToList args) $$(rhs)
            ||]
    , varA @"pat" <^> varA @"rhs"
        <:> semAct \(pat :* rhs :* HNil) ->
            [||Seq.singleton $ DeclVar $$(pat) $$(rhs)||]
    , varA @"gendecl"
        <:> semAct \(gendecl :* HNil) ->
            gendecl
    ]

rCdecls :: RuleExpr [Decl]
rCdecls = ruleExpr
    [ varA @"expbo" <^> varA @"cdeclsinl" <^> varA @"expbc"
        <:> semAct \(_ :* cdeclsinl :* _ :* HNil) ->
            cdeclsinl
    , varA @"impbo" <^> varA @"cdeclsinl" <^> varA @"impbc"
        <:> semAct \(_ :* cdeclsinl :* _ :* HNil) ->
            cdeclsinl
    ]

rCdeclsInL :: RuleExpr [Decl]
rCdeclsInL = ruleExpr
    [ varA @"(cdecl semi)* cdecl"
        <:> semAct \(cdeclsinl1 :* HNil) ->
            [||seqToList $$(cdeclsinl1)||]
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rCdeclsInL1 :: RuleExpr (Seq Decl)
rCdeclsInL1 = ruleExpr
    [ varA @"cdecl" <^> varA @"semi" <^> varA @"(cdecl semi)* cdecl"
        <:> semAct \(cdecl :* _ :* cdeclsinl1 :* HNil) ->
            [||$$(cdecl) Seq.>< $$(cdeclsinl1)||]
    , varA @"cdecl"
        <:> semAct \(cdecl :* HNil) ->
            cdecl
    ]

rCdecl :: RuleExpr (Seq Decl)
rCdecl = ruleExpr
    [ varA @"funlhs" <^> varA @"rhs"
        <:> semAct \(funlhs :* rhs :* HNil) ->
            [||case $$(funlhs) of
                (f, args) ->
                    Seq.singleton $ DeclFun f (seqToList args) $$(rhs)
            ||]
    , varA @"var" <^> varA @"rhs"
        <:> semAct \(var :* rhs :* HNil) ->
            [||Seq.singleton $ DeclVar (PatId $$(var) Nothing) $$(rhs)||]
    , varA @"gendecl"
        <:> semAct \(gendecl :* HNil) ->
            gendecl
    ]

rIdecls :: RuleExpr [Decl]
rIdecls = ruleExpr
    [ varA @"expbo" <^> varA @"ideclsinl" <^> varA @"expbc"
        <:> semAct \(_ :* ideclsinl :* _ :* HNil) ->
            ideclsinl
    , varA @"impbo" <^> varA @"ideclsinl" <^> varA @"impbc"
        <:> semAct \(_ :* ideclsinl :* _ :* HNil) ->
            ideclsinl
    ]

rIdeclsInL :: RuleExpr [Decl]
rIdeclsInL = ruleExpr
    [ varA @"(idecl semi)* idecl"
        <:> semAct \(ideclsinl1 :* HNil) ->
            [||seqToList $$(ideclsinl1)||]
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rIdeclsInL1 :: RuleExpr (Seq Decl)
rIdeclsInL1 = ruleExpr
    [ varA @"idecl" <^> varA @"semi" <^> varA @"(idecl semi)* idecl"
        <:> semAct \(idecl :* _ :* ideclsinl1 :* HNil) ->
            [||$$(idecl) Seq.>< $$(ideclsinl1)||]
    , varA @"idecl"
        <:> semAct \(idecl :* HNil) ->
            idecl
    ]

rIdecl :: RuleExpr (Seq Decl)
rIdecl = ruleExpr
    [ varA @"funlhs" <^> varA @"rhs"
        <:> semAct \(funlhs :* rhs :* HNil) ->
            [||case $$(funlhs) of
                (f, args) ->
                    Seq.singleton $ DeclFun f (seqToList args) $$(rhs)
            ||]
    , varA @"var" <^> varA @"rhs"
        <:> semAct \(var :* rhs :* HNil) ->
            [||Seq.singleton $ DeclVar (PatId $$(var) Nothing) $$(rhs)||]
    , eps
        $ semAct \HNil ->
            [||Seq.empty||]
    ]

rGenDecl :: RuleExpr (Seq Decl)
rGenDecl = ruleExpr
    [ varA @"vars" <^> tokA @"::" <^> varA @"(context '=>')?" <^> varA @"type"
        <:> semAct \(vars :* _ :* _ :* contextopt :* ty :* HNil) ->
            [||Seq.singleton $ DeclSig (seqToList $$(vars)) $$(contextopt) $$(ty)||]
    , varA @"fixity" <^> varA @"integer" <^> varA @"ops"
        <:> semAct \(fixity :* integer :* ops :* HNil) ->
            [||Seq.singleton $ DeclFixity $$(fixity) (Just (fromInteger $$(integer))) (seqToList $$(ops))||]
    , varA @"fixity" <^> varA @"ops"
        <:> semAct \(fixity :* ops :* HNil) ->
            [||Seq.singleton $ DeclFixity $$(fixity) Nothing (seqToList $$(ops))||]
    , eps
        $ semAct \HNil ->
            [||Seq.empty||]
    ]

rOps :: RuleExpr (Seq Id)
rOps = ruleExpr
    [ varA @"op" <^> tokA @"," <^> varA @"ops"
        <:> semAct \(op :* _ :* _ :* ops :* HNil) ->
            [||$$(op) Seq.:<| $$(ops)||]
    , varA @"op"
        <:> semAct \(op :* HNil) ->
            [||Seq.singleton $$(op)||]
    ]

rVars :: RuleExpr (Seq Id)
rVars = ruleExpr
    [ varA @"var" <^> tokA @"," <^> varA @"vars"
        <:> semAct \(var :* _ :* _ :* vars :* HNil) ->
            [||$$(var) Seq.:<| $$(vars)||]
    , varA @"var"
        <:> semAct \(var :* HNil) ->
            [||Seq.singleton $$(var)||]
    ]

rFixity :: RuleExpr Fixity
rFixity = ruleExpr
    [ tokA @"infixl"
        <:> semAct \(_ :* _ :* HNil) ->
            [||FixityInfixL||]
    , tokA @"infixr"
        <:> semAct \(_ :* _ :* HNil) ->
            [||FixityInfixR||]
    , tokA @"infix"
        <:> semAct \(_ :* _ :* HNil) ->
            [||FixityInfix||]
    ]

rType :: RuleExpr Type
rType = ruleExpr
    [ varA @"btype" <^> tokA @"->" <^> varA @"type"
        <:> semAct \(btype :* _ :* _ :* ty :* HNil) ->
            [||TypeArrow $$(btype) $$(ty)||]
    , varA @"btype"
        <:> semAct \(btype :* HNil) ->
            btype
    ]

rBtype :: RuleExpr Type
rBtype = ruleExpr
    [ varA @"atype" <^> varA @"atype*"
        <:> semAct \(atype :* atypes :* HNil) ->
            [||TypeApp $$(atype) (seqToList $$(atypes))||]
    ]

rAtypes :: RuleExpr (Seq Type)
rAtypes = ruleExpr
    [ varA @"atype" <^> varA @"atype*"
        <:> semAct \(atype :* atypes :* HNil) ->
            [||$$(atype) Seq.:<| $$(atypes)||]
    , eps
        $ semAct \HNil ->
            [||Seq.empty||]
    ]

rAtype :: RuleExpr Type
rAtype = ruleExpr
    [ varA @"gtycon"
        <:> semAct \(gtycon :* HNil) ->
            gtycon
    , varA @"tyvar"
        <:> semAct \(tyvar :* HNil) ->
            [||TypeId (nonQualifiedId $$(tyvar))||]
    , tokA @"(" <^> varA @"(type ',')+ type" <^> tokA @")"
        <:> semAct \(_ :* _ :* types2 :* _ :* _ :* HNil) ->
            [||TypeTuple (seqToList $$(types2))||]
    , tokA @"[" <^> varA @"type" <^> tokA @"]"
        <:> semAct \(_ :* _ :* ty :* _ :* _ :* HNil) ->
            [||TypeList $$(ty)||]
    , tokA @"(" <^> varA @"type" <^> tokA @")"
        <:> semAct \(_ :* _ :* ty :* _ :* _ :* HNil) ->
            ty
    ]

rTypes2 :: RuleExpr (Seq Type)
rTypes2 = ruleExpr
    [ varA @"type" <^> tokA @"," <^> varA @"(type ',')+ type"
        <:> semAct \(ty :* _ :* _ :* types2 :* HNil) ->
            [||$$(ty) Seq.:<| $$(types2)||]
    , varA @"type" <^> tokA @"," <^> varA @"type"
        <:> semAct \(ty1 :* _ :* _ :* ty2 :* HNil) ->
            [||Seq.fromList [$$(ty1), $$(ty2)]||]
    ]

rGtycon :: RuleExpr Type
rGtycon = ruleExpr
    [ varA @"qtycon"
        <:> semAct \(qtycon :* HNil) ->
            [||TypeId $$(qtycon)||]
    , tokA @"(" <^> tokA @")"
        <:> semAct \(_ :* _ :* _ :* _ :* HNil) ->
            [||TypeId $ nonQualifiedId $ mkId "()"||]
    , tokA @"[" <^> tokA @"]"
        <:> semAct \(_ :* _ :* _ :* _ :* HNil) ->
            [||TypeId $ nonQualifiedId $ mkId "[]"||]
    , tokA @"(" <^> tokA @"->" <^> tokA @")"
        <:> semAct \(_ :* _ :* _ :* _ :* _ :* _ :* HNil) ->
            [||TypeId $ nonQualifiedId $ mkId "->"||]
    , tokA @"(" <^> varA @"','+" <^> tokA @")"
        <:> semAct \(_ :* _ :* i :* _ :* _ :* HNil) ->
            [||TypeTupleCon $$(i)||]
    ]

rCommas1 :: RuleExpr Int
rCommas1 = ruleExpr
    [ tokA @"," <^> varA @"','+"
        <:> semAct \(_ :* _ :* i :* HNil) ->
            [||$$(i) + 1||]
    , tokA @","
        <:> semAct \(_ :* _ :* HNil) ->
            [||1||]
    ]

rContext :: RuleExpr Context
rContext = ruleExpr
    [ varA @"class"
        <:> semAct \(cls :* HNil) ->
            [||Context [$$(cls)]||]
    , tokA @"(" <^> varA @"((class ',')* class)?" <^> tokA @")"
        <:> semAct \(_ :* _ :* classes :* _ :* _ :* HNil) ->
            [||Context $$(classes)||]
    ]

rClasses :: RuleExpr [Type]
rClasses = ruleExpr
    [ varA @"(class ',')* class"
        <:> semAct \(classes1 :* HNil) ->
            [||seqToList $$(classes1)||]
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rClasses1 :: RuleExpr (Seq Type)
rClasses1 = ruleExpr
    [ varA @"class" <^> tokA @"," <^> varA @"(class ',')* class"
        <:> semAct \(cls :* _ :* _ :* classes1 :* HNil) ->
            [||$$(cls) Seq.:<| $$(classes1)||]
    , varA @"class"
        <:> semAct \(cls :* HNil) ->
            [||Seq.singleton $$(cls)||]
    ]

rClass :: RuleExpr Type
rClass = ruleExpr
    [ varA @"qtycon" <^> varA @"tyvar"
        <:> semAct \(qtycon :* tyvar :* HNil) ->
            [||TypeApp (TypeId $$(qtycon)) [TypeId (nonQualifiedId $$(tyvar))]||]
    , varA @"qtycon" <^> tokA @"(" <^> varA @"tyvar" <^> varA @"atype+" <^> tokA @")"
        <:> semAct \(qtycon :* _ :* _ :* tyvar :* atypes1 :* _ :* _ :* HNil) ->
            [||TypeApp
                (TypeId $$(qtycon))
                [TypeApp (TypeId (nonQualifiedId $$(tyvar))) (seqToList $$(atypes1))]
            ||]
    ]

rAtypes1 :: RuleExpr (Seq Type)
rAtypes1 = ruleExpr
    [ varA @"atype" <^> varA @"atype+"
        <:> semAct \(atype :* atypes1 :* HNil) ->
            [||$$(atype) Seq.:<| $$(atypes1)||]
    , varA @"atype"
        <:> semAct \(atype :* HNil) ->
            [||Seq.singleton $$(atype)||]
    ]

rScontext :: RuleExpr Context
rScontext = ruleExpr
    [ varA @"simpleclass"
        <:> semAct \(simpleclass :* HNil) ->
            [||Context [$$(simpleclass)]||]
    , tokA @"(" <^> varA @"((simpleclass ',')* simpleclass)?" <^> tokA @")"
        <:> semAct \(_ :* _ :* simpleclasses :* _ :* _ :* HNil) ->
            [||Context $$(simpleclasses)||]
    ]

rSimpleClasses :: RuleExpr [Type]
rSimpleClasses = ruleExpr
    [ varA @"(simpleclass ',')* simpleclass"
        <:> semAct \(simpleclasses1 :* HNil) ->
            [||seqToList $$(simpleclasses1)||]
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rSimpleClasses1 :: RuleExpr (Seq Type)
rSimpleClasses1 = ruleExpr
    [ varA @"simpleclass" <^> tokA @"," <^> varA @"(simpleclass ',')* simpleclass"
        <:> semAct \(simpleclass :* _ :* _ :* simpleclasses1 :* HNil) ->
            [||$$(simpleclass) Seq.:<| $$(simpleclasses1)||]
    , varA @"simpleclass"
        <:> semAct \(simpleclass :* HNil) ->
            [||Seq.singleton $$(simpleclass)||]
    ]

rSimpleClass :: RuleExpr Type
rSimpleClass = ruleExpr
    [ varA @"qtycon" <^> varA @"tyvar"
        <:> semAct \(qtycon :* tyvar :* HNil) ->
            [||TypeApp (TypeId $$(qtycon)) [TypeId (nonQualifiedId $$(tyvar))]||]
    ]

rSimpleType :: RuleExpr Type
rSimpleType = ruleExpr
    [ varA @"tycon" <^> varA @"tyvar*"
        <:> semAct \(tycon :* tyvars :* HNil) ->
            [||TypeApp (TypeId (nonQualifiedId $$(tycon))) (seqToList $$(tyvars))||]
    ]

rTyVars :: RuleExpr (Seq Type)
rTyVars = ruleExpr
    [ varA @"tyvar" <^> varA @"tyvar*"
        <:> semAct \(tyvar :* tyvars :* HNil) ->
            [||TypeId (nonQualifiedId $$(tyvar)) Seq.:<| $$(tyvars)||]
    , eps
        $ semAct \HNil ->
            [||Seq.empty||]
    ]

rConstrs :: RuleExpr (Seq Constr)
rConstrs = ruleExpr
    [ varA @"constr" <^> tokA @"|" <^> varA @"constrs"
        <:> semAct \(constr :* _ :* _ :* constrs :* HNil) ->
            [||$$(constr) Seq.:<| $$(constrs)||]
    , varA @"constr"
        <:> semAct \(constr :* HNil) ->
            [||Seq.singleton $$(constr)||]
    ]

rConstr :: RuleExpr Constr
rConstr = ruleExpr
    [ varA @"con" <^> varA @"expbo" <^> varA @"((fielddecl ',')* fielddecl)?" <^> varA @"expbc"
        <:> semAct \(con :* _ :* fielddecls :* _ :* HNil) ->
            [||ConstrWithFields $$(con) $$(fielddecls)||]
    , varA @"btype | '!' atype" <^> varA @"conop" <^> varA @"btype | '!' atype"
        <:> semAct \(abcty1 :* conop :* abcty2 :* HNil) ->
            [||ConstrApp $$(conop) [$$(abcty1), $$(abcty2)]||]
    , varA @"con" <^> varA @"('!'? atype)*"
        <:> semAct \(con :* actypes :* HNil) ->
            [||ConstrApp $$(con) (seqToList $$(actypes))||]
    ]

rFieldDecls :: RuleExpr [(Strictness, [Id], Type)]
rFieldDecls = ruleExpr
    [ varA @"(fielddecl ',')* fielddecl"
        <:> semAct \(fielddecls1 :* HNil) ->
            [||seqToList $$(fielddecls1)||]
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rFieldDecls1 :: RuleExpr (Seq (Strictness, [Id], Type))
rFieldDecls1 = ruleExpr
    [ varA @"fielddecl" <^> tokA @"," <^> varA @"(fielddecl ',')* fielddecl"
        <:> semAct \(fielddecl :* _ :* _ :* fielddecls1 :* HNil) ->
            [||$$(fielddecl) Seq.:<| $$(fielddecls1)||]
    , varA @"fielddecl"
        <:> semAct \(fielddecl :* HNil) ->
            [||Seq.singleton $$(fielddecl)||]
    ]

rAbctype :: RuleExpr (Strictness, Type)
rAbctype = ruleExpr
    [ varA @"btype"
        <:> semAct \(btype :* HNil) ->
            [||(Unstrict, $$(btype))||]
    , varA @"'!'" <^> varA @"atype"
        <:> semAct \(_ :* atype :* HNil) ->
            [||(Strict, $$(atype))||]
    ]

rActypes :: RuleExpr (Seq (Strictness, Type))
rActypes = ruleExpr
    [ varA @"'!'? atype" <^> varA @"('!'? atype)*"
        <:> semAct \(actype :* actypes :* HNil) ->
            [||$$(actype) Seq.:<| $$(actypes)||]
    , eps
        $ semAct \HNil ->
            [||Seq.empty||]
    ]

rActype :: RuleExpr (Strictness, Type)
rActype = ruleExpr
    [ varA @"'!'" <^> varA @"atype"
        <:> semAct \(_ :* atype :* HNil) ->
            [||(Strict, $$(atype))||]
    , varA @"atype"
        <:> semAct \(atype :* HNil) ->
            [||(Unstrict, $$(atype))||]
    ]

rNewConstr :: RuleExpr Constr
rNewConstr = ruleExpr
    [ varA @"con" <^> varA @"expbo" <^> varA @"var" <^> tokA @"::" <^> varA @"type" <^> varA @"expbc"
        <:> semAct \(con :* _ :* var :* _ :* _ :* ty :* _ :* HNil) ->
            [||ConstrWithFields $$(con) [(Unstrict, [$$(var)], $$(ty))]||]
    , varA @"con" <^> varA @"atype"
        <:> semAct \(con :* atype :* HNil) ->
            [||ConstrApp $$(con) [(Unstrict, $$(atype))]||]
    ]

rFieldDecl :: RuleExpr (Strictness, [Id], Type)
rFieldDecl = ruleExpr
    [ varA @"vars" <^> tokA @"::" <^> varA @"type"
        <:> semAct \(vars :* _ :* _ :* ty :* HNil) ->
            [||(Unstrict, seqToList $$(vars), $$(ty))||]
    , varA @"vars" <^> tokA @"::" <^> varA @"'!'" <^> varA @"atype"
        <:> semAct \(vars :* _ :* _ :* _ :* atype :* HNil) ->
            [||(Strict, seqToList $$(vars), $$(atype))||]
    ]

rDeriving :: RuleExpr Deriving
rDeriving = ruleExpr
    [ tokA @"deriving" <^> varA @"dclass"
        <:> semAct \(_ :* _ :* dclass :* HNil) ->
            [||Deriving [$$(dclass)]||]
    , tokA @"deriving" <^> tokA @"(" <^> varA @"((dclass ',')* dclass)?" <^> tokA @")"
        <:> semAct \(_ :* _ :* _ :* _ :* dclasses :* _ :* _ :* HNil) ->
            [||Deriving $$(dclasses)||]
    ]

rDclasses :: RuleExpr [Type]
rDclasses = ruleExpr
    [ varA @"(dclass ',')* dclass"
        <:> semAct \(dclasses1 :* HNil) ->
            [||seqToList $$(dclasses1)||]
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rDclasses1 :: RuleExpr (Seq Type)
rDclasses1 = ruleExpr
    [ varA @"dclass" <^> tokA @"," <^> varA @"(dclass ',')* dclass"
        <:> semAct \(dclass :* _ :* _ :* dclasses1 :* HNil) ->
            [||$$(dclass) Seq.:<| $$(dclasses1)||]
    , varA @"dclass"
        <:> semAct \(dclass :* HNil) ->
            [||Seq.singleton $$(dclass)||]
    ]

rDclass :: RuleExpr Type
rDclass = ruleExpr
    [ varA @"qtycon"
        <:> semAct \(qtycon :* HNil) ->
            [||TypeId $$(qtycon)||]
    ]

rInst :: RuleExpr Type
rInst = ruleExpr
    [ varA @"gtycon"
        <:> semAct \(gtycon :* HNil) ->
            gtycon
    , tokA @"(" <^> varA @"gtycon" <^> varA @"tyvar*" <^> tokA @")"
        <:> semAct \(_ :* _ :* gtycon :* tyvars :* _ :* _ :* HNil) ->
            [||TypeApp $$(gtycon) (seqToList $$(tyvars))||]
    , tokA @"(" <^> varA @"(tyvar ',')+ tyvar" <^> tokA @")"
        <:> semAct \(_ :* _ :* tyvars2 :* _ :* _ :* HNil) ->
            [||TypeTuple (seqToList $$(tyvars2))||]
    , tokA @"(" <^> varA @"tyvar" <^> tokA @")"
        <:> semAct \(_ :* _ :* tyvar :* _ :* _ :* HNil) ->
            [||TypeId (nonQualifiedId $$(tyvar))||]
    , tokA @"[" <^> varA @"tyvar" <^> tokA @"]"
        <:> semAct \(_ :* _ :* tyvar :* _ :* _ :* HNil) ->
            [||TypeList (TypeId (nonQualifiedId $$(tyvar)))||]
    , tokA @"(" <^> varA @"tyvar" <^> tokA @"->" <^> varA @"tyvar" <^> tokA @")"
        <:> semAct \(_ :* _ :* tyvar1 :* _ :* _ :* tyvar2 :* _ :* _ :* HNil) ->
            [||TypeArrow (TypeId (nonQualifiedId $$(tyvar1))) (TypeId (nonQualifiedId $$(tyvar2)))||]
    ]

rTyVars2 :: RuleExpr (Seq Type)
rTyVars2 = ruleExpr
    [ varA @"tyvar" <^> tokA @"," <^> varA @"(tyvar ',')+ tyvar"
        <:> semAct \(tyvar :* _ :* _ :* tyvars2 :* HNil) ->
            [||TypeId (nonQualifiedId $$(tyvar)) Seq.:<| $$(tyvars2)||]
    , varA @"tyvar" <^> tokA @"," <^> varA @"tyvar"
        <:> semAct \(tyvar1 :* _ :* _ :* tyvar2 :* HNil) ->
            [||Seq.fromList [TypeId (nonQualifiedId $$(tyvar1)), TypeId (nonQualifiedId $$(tyvar2))]||]
    ]

rFdecl :: RuleExpr Decl
rFdecl = ruleExpr
    [ tokA @"import" <^> varA @"callconv" <^> varA @"safety?" <^> varA @"impent" <^> varA @"var" <^> tokA @"::" <^> varA @"ftype"
        <:> semAct \(_ :* _ :* callconv :* safetyopt :* impent :* var :* _ :* _ :* ftype :* HNil) ->
            [||DeclForeignImport $$(callconv) $$(safetyopt) $$(impent) $$(var) $$(ftype)||]
    , varA @"'export'" <^> varA @"callconv" <^> varA @"expent" <^> varA @"var" <^> tokA @"::" <^> varA @"ftype"
        <:> semAct \(_ :* callconv :* expent :* var :* _ :* _ :* ftype :* HNil) ->
            [||DeclForeignExport $$(callconv) $$(expent) $$(var) $$(ftype)||]
    ]

rCallConv :: RuleExpr ForeignCallConv
rCallConv = ruleExpr
    [ varA @"varid"
        <:> semActM \(varid :* HNil) ->
            [||case $$(varid) of
                Id txt | txt == Text.pack "ccall" ->
                    pure ForeignCallCcall
                Id txt | txt == Text.pack "stdcall" ->
                    pure ForeignCallStdcall
                Id txt | txt == Text.pack "cplusplus" ->
                    pure ForeignCallCplusplus
                Id txt | txt == Text.pack "jvm" ->
                    pure ForeignCallJvm
                Id txt | txt == Text.pack "dotnet" ->
                    pure ForeignCallDotnet
                _ ->
                    failAction
            ||]
    ]

rSafetyOpt :: RuleExpr (Maybe Safety)
rSafetyOpt = ruleExpr
    [ varA @"safety"
        <:> semAct \(safety :* HNil) ->
            [||Just $$(safety)||]
    , eps
        $ semAct \HNil ->
            [||Nothing||]
    ]

rImpent :: RuleExpr (Maybe String)
rImpent = ruleExpr
    [ varA @"string"
        <:> semAct \(string :* HNil) ->
            [||Just $$(string)||]
    , eps
        $ semAct \HNil ->
            [||Nothing||]
    ]

rExpent :: RuleExpr (Maybe String)
rExpent = ruleExpr
    [ varA @"string"
        <:> semAct \(string :* HNil) ->
            [||Just $$(string)||]
    , eps
        $ semAct \HNil ->
            [||Nothing||]
    ]

rSafety :: RuleExpr Safety
rSafety = ruleExpr
    [ varA @"varid"
        <:> semActM \(varid :* HNil) ->
            [||case $$(varid) of
                Id txt | txt == Text.pack "safe" ->
                    pure Safe
                Id txt | txt == Text.pack "unsafe" ->
                    pure Unsafe
                _ ->
                    failAction
            ||]
    ]

rFtype :: RuleExpr Type
rFtype = ruleExpr
    [ varA @"fatype" <^> tokA @"->" <^> varA @"ftype"
        <:> semAct \(fatype :* _ :* _ :* ftype :* HNil) ->
            [||TypeArrow $$(fatype) $$(ftype)||]
    , varA @"frtype"
        <:> semAct \(frtype :* HNil) ->
            frtype
    ]

rFrtype :: RuleExpr Type
rFrtype = ruleExpr
    [ varA @"fatype"
        <:> semAct \(fatype :* HNil) ->
            fatype
    , tokA @"(" <^> tokA @")"
        <:> semAct \(_ :* _ :* _ :* _ :* HNil) ->
            [||TypeId (nonQualifiedId (mkId "()"))||]
    ]

rFatype :: RuleExpr Type
rFatype = ruleExpr
    [ varA @"qtycon" <^> varA @"atype*"
        <:> semAct \(qtycon :* atypes :* HNil) ->
            [||TypeApp (TypeId $$(qtycon)) (seqToList $$(atypes))||]
    ]

rFunlhs :: RuleExpr (Id, Seq Pat)
rFunlhs = ruleExpr
    [ varA @"var" <^> varA @"apat+"
        <:> semAct \(var :* apats1 :* HNil) ->
            [||($$(var), $$(apats1))||]
    , varA @"pat" <^> varA @"varop" <^> varA @"pat"
        <:> semAct \(pat1 :* varop :* pat2 :* HNil) ->
            [||($$(varop), Seq.fromList [$$(pat1), $$(pat2)])||]
    , tokA @"(" <^> varA @"funlhs" <^> tokA @")" <^> varA @"apat+"
        <:> semAct \(_ :* _ :* funlhs :* _ :* _ :* apats1 :* HNil) ->
            [||case $$(funlhs) of
                (v, pats) -> (v, pats Seq.>< $$(apats1))
            ||]
    ]

rApats1 :: RuleExpr (Seq Pat)
rApats1 = ruleExpr
    [ varA @"apat" <^> varA @"apat+"
        <:> semAct \(apat :* apats1 :* HNil) ->
            [||$$(apat) Seq.:<| $$(apats1)||]
    , varA @"apat"
        <:> semAct \(apat :* HNil) ->
            [||Seq.singleton $$(apat)||]
    ]

rRhs :: RuleExpr Rhs
rRhs = ruleExpr
    [ tokA @"=" <^> varA @"exp" <^> varA @"('where' decls)?"
        <:> semAct \(_ :* _ :* exp :* decls :* HNil) ->
            [||Rhs [([], $$(exp))] $$(decls)||]
    , varA @"gdrhs" <^> varA @"('where' decls)?"
        <:> semAct \(gdrhs :* decls :* HNil) ->
            [||Rhs (seqToList $$(gdrhs)) $$(decls)||]
    ]

rWhereDeclsOpt :: RuleExpr [Decl]
rWhereDeclsOpt = ruleExpr
    [ tokA @"where" <^> varA @"decls"
        <:> semAct \(_ :* _ :* decls :* HNil) ->
            decls
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rGdrhs :: RuleExpr (Seq ([Guard], Exp))
rGdrhs = ruleExpr
    [ varA @"guards" <^> tokA @"=" <^> varA @"exp" <^> varA @"gdrhs"
        <:> semAct \(guards :* _ :* _ :* exp :* gdrhs :* HNil) ->
            [||($$(guards), $$(exp)) Seq.:<| $$(gdrhs)||]
    , varA @"guards" <^> tokA @"=" <^> varA @"exp"
        <:> semAct \(guards :* _ :* _ :* exp :* HNil) ->
            [||Seq.singleton ($$(guards), $$(exp))||]
    ]

rGuards :: RuleExpr [Guard]
rGuards = ruleExpr
    [ tokA @"|" <^> varA @"(guard ',')* guard"
        <:> semAct \(_ :* _ :* guards1 :* HNil) ->
            [||seqToList $$(guards1)||]
    ]

rGuards1 :: RuleExpr (Seq Guard)
rGuards1 = ruleExpr
    [ varA @"guard" <^> tokA @"," <^> varA @"(guard ',')* guard"
        <:> semAct \(guard :* _ :* _ :* guards1 :* HNil) ->
            [||$$(guard) Seq.:<| $$(guards1)||]
    , varA @"guard"
        <:> semAct \(guard :* HNil) ->
            [||Seq.singleton $$(guard)||]
    ]

rGuard :: RuleExpr Guard
rGuard = ruleExpr
    [ varA @"pat" <^> tokA @"<-" <^> varA @"infixexp"
        <:> semAct \(pat :* _ :* _ :* infixexp :* HNil) ->
            [||GuardPat $$(pat) $$(infixexp)||]
    , tokA @"let" <^> varA @"decls"
        <:> semAct \(_ :* _ :* decls :* HNil) ->
            [||GuardLet $$(decls)||]
    , varA @"infixexp"
        <:> semAct \(infixexp :* HNil) ->
            [||GuardExp $$(infixexp)||]
    ]

rExp :: RuleExpr Exp
rExp = ruleExpr
    [ varA @"infixexp" <^> tokA @"::" <^> varA @"(context '=>')?" <^> varA @"type"
        <:> semAct \(infixexp :* _ :* _ :* contextopt :* ty :* HNil) ->
            [||ExpSig $$(infixexp) $$(contextopt) $$(ty)||]
    , varA @"infixexp"
        <:> semAct \(infixexp :* HNil) ->
            infixexp
    ]

rInfixExp :: RuleExpr Exp
rInfixExp = ruleExpr
    [ tokA @"-" <^> varA @"infixexp"
        <:> semAct \(_ :* _ :* infixexp :* HNil) ->
            [||ExpMinus $$(infixexp)||]
    , varA @"lexp" <^> varA @"qop" <^> varA @"infixexp"
        <:> semAct \(lexp :* qop :* infixexp :* HNil) ->
            [||ExpInfixApp $$(lexp) $$(qop) $$(infixexp)||]
    , varA @"lexp"
        <:> semAct \(lexp :* HNil) ->
            lexp
    ]

rLexp :: RuleExpr Exp
rLexp = ruleExpr
    [ tokA @"\\" <^> varA @"apat+" <^> tokA @"->" <^> varA @"exp"
        <:> semAct \(_ :* _ :* apats1 :* _ :* _ :* exp :* HNil) ->
            [||ExpLambda (seqToList $$(apats1)) $$(exp)||]
    , tokA @"let" <^> varA @"decls" <^> tokA @"in" <^> varA @"exp"
        <:> semAct \(_ :* _ :* decls :* _ :* _ :* exp :* HNil) ->
            [||ExpLet $$(decls) $$(exp)||]
    , tokA @"if" <^> varA @"exp" <^> varA @"semi?" <^> tokA @"then" <^> varA @"exp" <^> varA @"semi?" <^> tokA @"else" <^> varA @"exp"
        <:> semAct \(_ :* _ :* exp1 :* _ :* _ :* _ :* exp2 :* _ :* _ :* _ :* exp3 :* HNil) ->
            [||ExpIf $$(exp1) $$(exp2) $$(exp3)||]
    , tokA @"case" <^> varA @"exp" <^> tokA @"of" <^> varA @"casealts"
        <:> semAct \(_ :* _ :* exp :* _ :* _ :* casealts :* HNil) ->
            [||ExpCase $$(exp) $$(casealts)||]
    , tokA @"do" <^> varA @"dostmts"
        <:> semAct \(_ :* _ :* dostmts :* HNil) ->
            [||case $$(dostmts) of
                (stmts, exp) -> ExpDo stmts exp
            ||]
    , varA @"fexp"
        <:> semAct \(fexp :* HNil) ->
            fexp
    ]

rSemiOpt :: RuleExpr ()
rSemiOpt = ruleExpr
    [ varA @"semi"
        <:> semAct \(_ :* HNil) ->
            [||()||]
    , eps
        $ semAct \HNil ->
            [||()||]
    ]

rFexp :: RuleExpr Exp
rFexp = ruleExpr
    [ varA @"aexp" <^> varA @"aexp*"
        <:> semAct \(aexp :* aexps :* HNil) ->
            [||ExpApp $$(aexp) (seqToList $$(aexps))||]
    ]

rAexps :: RuleExpr (Seq Exp)
rAexps = ruleExpr
    [ varA @"aexp" <^> varA @"aexp*"
        <:> semAct \(aexp :* aexps :* HNil) ->
            [||$$(aexp) Seq.:<| $$(aexps)||]
    , eps
        $ semAct \HNil ->
            [||Seq.empty||]
    ]

rAexp :: RuleExpr Exp
rAexp = ruleExpr
    [ varA @"qcon" <^> varA @"expbo" <^> varA @"((fbind ',')* fbind)?" <^> varA @"expbc"
        <:> semAct \(qcon :* _ :* fbinds :* _ :* HNil) ->
            [||ExpRecordCon $$(qcon) $$(fbinds)||]
    , varA @"aexp2" <^> varA @"(expbo ((fbind ',')* fbind)? expbc)*"
        <:> semAct \(aexp2 :* recUpdates :* HNil) ->
            [||seqFoldl'
                do \aexp recUpdate -> ExpRecordUpdate aexp recUpdate
                do $$(aexp2)
                do $$(recUpdates)
            ||]
    ]

rRecUpdates :: RuleExpr (Seq [(QualifiedId, Exp)])
rRecUpdates = ruleExpr
    [ varA @"expbo" <^> varA @"((fbind ',')* fbind)?" <^> varA @"expbc" <^> varA @"(expbo ((fbind ',')* fbind)? expbc)*"
        <:> semAct \(_ :* fbinds :* _ :* recUpdates :* HNil) ->
            [||$$(fbinds) Seq.:<| $$(recUpdates)||]
    , eps
        $ semAct \HNil ->
            [||Seq.empty||]
    ]

rAexp2 :: RuleExpr Exp
rAexp2 = ruleExpr
    [ varA @"literal"
        <:> semAct \(literal :* HNil) ->
            [||ExpLit $$(literal)||]
    , tokA @"(" <^> varA @"exp" <^> tokA @")"
        <:> semAct \(_ :* _ :* exp :* _ :* _ :* HNil) ->
            exp
    , tokA @"(" <^> varA @"(exp ',')+ exp" <^> tokA @")"
        <:> semAct \(_ :* _ :* exps2 :* _ :* _ :* HNil) ->
            [||ExpTuple (seqToList $$(exps2))||]
    , tokA @"[" <^> varA @"(exp ',')* exp" <^> tokA @"]"
        <:> semAct \(_ :* _ :* exps1 :* _ :* _ :* HNil) ->
            [||ExpList (seqToList $$(exps1))||]
    , tokA @"[" <^> varA @"exp" <^> varA @"(',' exp)?" <^> tokA @".." <^> varA @"exp?" <^> tokA @"]"
        <:> semAct \(_ :* _ :* exp :* cexpopt :* _ :* _ :* expopt :* _ :* _ :* HNil) ->
            [||ExpListRange $$(exp) $$(cexpopt) $$(expopt)||]
    , tokA @"[" <^> varA @"exp" <^> tokA @"|" <^> varA @"(qual ',')* qual" <^> tokA @"]"
        <:> semAct \(_ :* _ :* exp :* _ :* _ :* quals1 :* _ :* _ :* HNil) ->
            [||ExpListComp $$(exp) (seqToList $$(quals1))||]
    , tokA @"(" <^> varA @"infixexp" <^> varA @"qop" <^> tokA @")"
        <:> semAct \(_ :* _ :* infixexp :* qop :* _ :* _ :* HNil) ->
            [||ExpSection (Just $$(infixexp)) $$(qop) Nothing||]
    -- `"(" exp ")"` includes `"(" "-" infixexp ")"`
    , tokA @"(" <^> varA @"qop" <^> varA @"infixexp" <^> tokA @")"
        <:> semAct \(_ :* _ :* qop :* infixexp :* _ :* _ :* HNil) ->
            [||ExpSection Nothing $$(qop) (Just $$(infixexp))||]
    , varA @"qvar"
        <:> semAct \(qvar :* HNil) ->
            [||ExpId $$(qvar)||]
    , varA @"gcon"
        <:> semAct \(gcon :* HNil) ->
            [||case $$(gcon) of
                GconId con ->
                    ExpId con
                GconTuple i ->
                    ExpTupleCon i
            ||]
    ]

rExps2 :: RuleExpr (Seq Exp)
rExps2 = ruleExpr
    [ varA @"exp" <^> tokA @"," <^> varA @"(exp ',')* exp"
        <:> semAct \(exp :* _ :* _ :* exps1 :* HNil) ->
            [||$$(exp) Seq.:<| $$(exps1)||]
    ]

rExps1 :: RuleExpr (Seq Exp)
rExps1 = ruleExpr
    [ varA @"exp" <^> tokA @"," <^> varA @"(exp ',')* exp"
        <:> semAct \(exp :* _ :* _ :* exps1 :* HNil) ->
            [||$$(exp) Seq.:<| $$(exps1)||]
    , varA @"exp"
        <:> semAct \(exp :* HNil) ->
            [||Seq.singleton $$(exp)||]
    ]

rCexpOpt :: RuleExpr (Maybe Exp)
rCexpOpt = ruleExpr
    [ tokA @"," <^> varA @"exp"
        <:> semAct \(_ :* _ :* exp :* HNil) ->
            [||Just $$(exp)||]
    , eps
        $ semAct \HNil ->
            [||Nothing||]
    ]

rExpOpt :: RuleExpr (Maybe Exp)
rExpOpt = ruleExpr
    [ varA @"exp"
        <:> semAct \(exp :* HNil) ->
            [||Just $$(exp)||]
    , eps
        $ semAct \HNil ->
            [||Nothing||]
    ]

rQuals1 :: RuleExpr (Seq Guard)
rQuals1 = ruleExpr
    [ varA @"qual" <^> tokA @"," <^> varA @"(qual ',')* qual"
        <:> semAct \(qual :* _ :* _ :* quals1 :* HNil) ->
            [||$$(qual) Seq.:<| $$(quals1)||]
    , varA @"qual"
        <:> semAct \(qual :* HNil) ->
            [||Seq.singleton $$(qual)||]
    ]

rFbinds :: RuleExpr [(QualifiedId, Exp)]
rFbinds = ruleExpr
    [ varA @"(fbind ',')* fbind"
        <:> semAct \(fbinds1 :* HNil) ->
            [||seqToList $$(fbinds1)||]
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rFbinds1 :: RuleExpr (Seq (QualifiedId, Exp))
rFbinds1 = ruleExpr
    [ varA @"fbind" <^> tokA @"," <^> varA @"(fbind ',')* fbind"
        <:> semAct \(fbind :* _ :* _ :* fbinds1 :* HNil) ->
            [||$$(fbind) Seq.:<| $$(fbinds1)||]
    , varA @"fbind"
        <:> semAct \(fbind :* HNil) ->
            [||Seq.singleton $$(fbind)||]
    ]

rQual :: RuleExpr Guard
rQual = ruleExpr
    [ varA @"pat" <^> tokA @"<-" <^> varA @"exp"
        <:> semAct \(pat :* _ :* _ :* exp :* HNil) ->
            [||GuardPat $$(pat) $$(exp)||]
    , tokA @"let" <^> varA @"decls"
        <:> semAct \(_ :* _ :* decls :* HNil) ->
            [||GuardLet $$(decls)||]
    , varA @"exp"
        <:> semAct \(exp :* HNil) ->
            [||GuardExp $$(exp)||]
    ]

rCaseAlts :: RuleExpr [CaseAlt]
rCaseAlts = ruleExpr
    [ varA @"expbo" <^> varA @"alts" <^> varA @"expbc"
        <:> semAct \(_ :* alts :* _ :* HNil) ->
            [||seqToList $$(alts)||]
    , varA @"impbo" <^> varA @"alts" <^> varA @"impbc"
        <:> semAct \(_ :* alts :* _ :* HNil) ->
            [||seqToList $$(alts)||]
    ]

rAlts :: RuleExpr (Seq CaseAlt)
rAlts = ruleExpr
    [ varA @"alt" <^> varA @"semi" <^> varA @"alts"
        <:> semAct \(alt :* _ :* alts :* HNil) ->
            [||$$(alt) Seq.>< $$(alts)||]
    , varA @"alt"
        <:> semAct \(alt :* HNil) ->
            alt
    ]

rAlt :: RuleExpr (Seq CaseAlt)
rAlt = ruleExpr
    [ varA @"pat" <^> tokA @"->" <^> varA @"exp" <^> varA @"('where' decls)?"
        <:> semAct \(pat :* _ :* _ :* exp :* decls :* HNil) ->
            [||Seq.singleton $ CaseAlt $$(pat) [([], $$(exp))] $$(decls)||]
    , varA @"pat" <^> varA @"gdpat" <^> varA @"('where' decls)?"
        <:> semAct \(pat :* gdpat :* decls :* HNil) ->
            [||Seq.singleton $ CaseAlt $$(pat) (seqToList $$(gdpat)) $$(decls)||]
    , eps
        $ semAct \HNil ->
            [||Seq.empty||]
    ]

rGdpat :: RuleExpr (Seq ([Guard], Exp))
rGdpat = ruleExpr
    [ varA @"guards" <^> tokA @"->" <^> varA @"exp" <^> varA @"gdpat"
        <:> semAct \(guards :* _ :* _ :* exp :* gdpat :* HNil) ->
            [||($$(guards), $$(exp)) Seq.:<| $$(gdpat)||]
    , varA @"guards" <^> tokA @"->" <^> varA @"exp"
        <:> semAct \(guards :* _ :* _ :* exp :* HNil) ->
            [||Seq.singleton ($$(guards), $$(exp))||]
    ]

rDoStmts :: RuleExpr ([Stmt], Exp)
rDoStmts = ruleExpr
    [ varA @"expbo" <^> varA @"stmts" <^> varA @"expbc"
        <:> semAct \(_ :* stmts :* _ :* HNil) ->
            stmts
    , varA @"impbo" <^> varA @"stmts" <^> varA @"impbc"
        <:> semAct \(_ :* stmts :* _ :* HNil) ->
            stmts
    ]

rStmts :: RuleExpr ([Stmt], Exp)
rStmts = ruleExpr
    [ varA @"stmt*" <^> varA @"exp" <^> varA @"semi?"
        <:> semAct \(stmts0 :* exp :* _ :* HNil) ->
            [||(seqToList $$(stmts0), $$(exp))||]
    ]

rStmts0 :: RuleExpr (Seq Stmt)
rStmts0 = ruleExpr
    [ varA @"stmt" <^> varA @"stmt*"
        <:> semAct \(stmt :* stmts0 :* HNil) ->
            [||$$(stmt) Seq.>< $$(stmts0)||]
    , eps
        $ semAct \HNil ->
            [||Seq.empty||]
    ]

rStmt :: RuleExpr (Seq Stmt)
rStmt = ruleExpr
    [ varA @"exp" <^> varA @"semi"
        <:> semAct \(exp :* _ :* HNil) ->
            [||Seq.singleton $ StmtExp $$(exp)||]
    , varA @"pat" <^> tokA @"<-" <^> varA @"exp" <^> varA @"semi"
        <:> semAct \(pat :* _ :* _ :* exp :* _ :* HNil) ->
            [||Seq.singleton $ StmtPat $$(pat) $$(exp)||]
    , tokA @"let" <^> varA @"decls" <^> varA @"semi"
        <:> semAct \(_ :* _ :* decls :* _ :* HNil) ->
            [||Seq.singleton $ StmtLet $$(decls)||]
    , varA @"semi"
        <:> semAct \(_ :* HNil) ->
            [||Seq.empty||]
    ]

rFbind :: RuleExpr (QualifiedId, Exp)
rFbind = ruleExpr
    [ varA @"qvar" <^> tokA @"=" <^> varA @"exp"
        <:> semAct \(qvar :* _ :* _ :* exp :* HNil) ->
            [||($$(qvar), $$(exp))||]
    ]

rPat :: RuleExpr Pat
rPat = ruleExpr
    [ varA @"lpat" <^> varA @"qconop" <^> varA @"pat"
        <:> semAct \(lpat :* qconop :* pat :* HNil) ->
            [||PatInfixApp $$(lpat) $$(qconop) $$(pat)||]
    , varA @"lpat"
        <:> semAct \(lpat :* HNil) ->
            lpat
    ]

rLpat :: RuleExpr Pat
rLpat = ruleExpr
    [ tokA @"-" <^> varA @"integer"
        <:> semAct \(_ :* _ :* integer :* HNil) ->
            [||PatMinusInteger $$(integer)||]
    , tokA @"-" <^> varA @"float"
        <:> semAct \(_ :* _ :* float :* HNil) ->
            [||PatMinusFloat $$(float)||]
    , varA @"gcon" <^> varA @"apat+"
        <:> semAct \(gcon :* apats1 :* HNil) ->
            [||PatApp $$(gcon) (seqToList $$(apats1))||]
    , varA @"apat"
        <:> semAct \(apat :* HNil) ->
            apat
    ]

rApat :: RuleExpr Pat
rApat = ruleExpr
    [ varA @"var" <^> tokA @"@" <^> varA @"apat"
        <:> semAct \(var :* _ :* _ :* apat :* HNil) ->
            [||PatId $$(var) (Just $$(apat))||]
    , varA @"var"
        <:> semAct \(var :* HNil) ->
            [||PatId $$(var) Nothing||]
    , varA @"literal"
        <:> semAct \(literal :* HNil) ->
            [||PatLit $$(literal)||]
    , tokA @"_"
        <:> semAct \(_ :* _ :* HNil) ->
            [||PatWildcard||]
    , tokA @"(" <^> varA @"pat" <^> tokA @")"
        <:> semAct \(_ :* _ :* pat :* _ :* _ :* HNil) ->
            pat
    , tokA @"(" <^> varA @"(pat ',')+ pat" <^> tokA @")"
        <:> semAct \(_ :* _ :* pats2 :* _ :* _ :* HNil) ->
            [||PatTuple (seqToList $$(pats2))||]
    , tokA @"[" <^> varA @"(pat ',')* pat" <^> tokA @"]"
        <:> semAct \(_ :* _ :* pats1 :* _ :* _ :* HNil) ->
            [||PatList (seqToList $$(pats1))||]
    , tokA @"~" <^> varA @"apat"
        <:> semAct \(_ :* _ :* apat :* HNil) ->
            [||PatLazy $$(apat)||]
    , varA @"qcon" <^> varA @"expbo" <^> varA @"((fpat ',')* fpat)?" <^> varA @"expbc"
        <:> semAct \(qcon :* _ :* fpats :* _ :* HNil) ->
            [||PatRecord $$(qcon) $$(fpats)||]
    , varA @"gcon"
        <:> semAct \(gcon :* HNil) ->
            [||PatCon $$(gcon)||]
    ]

rPats2 :: RuleExpr (Seq Pat)
rPats2 = ruleExpr
    [ varA @"pat" <^> tokA @"," <^> varA @"(pat ',')* pat"
        <:> semAct \(pat :* _ :* _ :* pats1 :* HNil) ->
            [||$$(pat) Seq.:<| $$(pats1)||]
    ]

rPats1 :: RuleExpr (Seq Pat)
rPats1 = ruleExpr
    [ varA @"pat" <^> tokA @"," <^> varA @"(pat ',')* pat"
        <:> semAct \(pat :* _ :* _ :* pats1 :* HNil) ->
            [||$$(pat) Seq.:<| $$(pats1)||]
    , varA @"pat"
        <:> semAct \(pat :* HNil) ->
            [||Seq.fromList [$$(pat)]||]
    ]

rFpats :: RuleExpr [(QualifiedId, Pat)]
rFpats = ruleExpr
    [ varA @"(fpat ',')* fpat"
        <:> semAct \(fpats1 :* HNil) ->
            [||seqToList $$(fpats1)||]
    , eps
        $ semAct \HNil ->
            [||[]||]
    ]

rFpats1 :: RuleExpr (Seq (QualifiedId, Pat))
rFpats1 = ruleExpr
    [ varA @"fpat" <^> tokA @"," <^> varA @"(fpat ',')* fpat"
        <:> semAct \(fpat :* _ :* _ :* fpats1 :* HNil) ->
            [||$$(fpat) Seq.:<| $$(fpats1)||]
    , varA @"fpat"
        <:> semAct \(fpat :* HNil) ->
            [||Seq.singleton $$(fpat)||]
    ]

rFpat :: RuleExpr (QualifiedId, Pat)
rFpat = ruleExpr
    [ varA @"qvar" <^> tokA @"=" <^> varA @"pat"
        <:> semAct \(qvar :* _ :* _ :* pat :* HNil) ->
            [||($$(qvar), $$(pat))||]
    ]

rGcon :: RuleExpr Gcon
rGcon = ruleExpr
    [ tokA @"(" <^> tokA @")"
        <:> semAct \(_ :* _ :* _ :* _ :* HNil) ->
            [||GconId $ nonQualifiedId (mkId "()")||]
    , tokA @"[" <^> tokA @"]"
        <:> semAct \(_ :* _ :* _ :* _ :* HNil) ->
            [||GconId $ nonQualifiedId (mkId "[]")||]
    , tokA @"(" <^> varA @"','+" <^> tokA @")"
        <:> semAct \(_ :* _ :* i :* _ :* _ :* HNil) ->
            [||GconTuple $$(i)||]
    , varA @"qcon"
        <:> semAct \(qcon :* HNil) ->
            [||GconId $$(qcon)||]
    ]

rVar :: RuleExpr Id
rVar = ruleExpr
    [ varA @"varid"
        <:> semAct \(varid :* HNil) ->
            varid
    , tokA @"(" <^> varA @"varsym" <^> tokA @")"
        <:> semAct \(_ :* _ :* varsym :* _ :* _ :* HNil) ->
            varsym
    ]

rQvar :: RuleExpr QualifiedId
rQvar = ruleExpr
    [ varA @"qvarid"
        <:> semAct \(qvarid :* HNil) ->
            qvarid
    , tokA @"(" <^> varA @"qvarsym" <^> tokA @")"
        <:> semAct \(_ :* _ :* qvarsym :* _ :* _ :* HNil) ->
            qvarsym
    ]

rCon :: RuleExpr Id
rCon = ruleExpr
    [ varA @"conid"
        <:> semAct \(conid :* HNil) ->
            conid
    , tokA @"(" <^> varA @"consym" <^> tokA @")"
        <:> semAct \(_ :* _ :* consym :* _ :* _ :* HNil) ->
            consym
    ]

rQcon :: RuleExpr QualifiedId
rQcon = ruleExpr
    [ varA @"qconid"
        <:> semAct \(qconid :* HNil) ->
            qconid
    , tokA @"(" <^> varA @"gconsym" <^> tokA @")"
        <:> semAct \(_ :* _ :* qconsym :* _ :* _ :* HNil) ->
            qconsym
    ]

rVarOp :: RuleExpr Id
rVarOp = ruleExpr
    [ varA @"varsym"
        <:> semAct \(varsym :* HNil) ->
            varsym
    , tokA @"`" <^> varA @"varid" <^> tokA @"`"
        <:> semAct \(_ :* _ :* varid :* _ :* _ :* HNil) ->
            varid
    ]

rQvarOp :: RuleExpr QualifiedId
rQvarOp = ruleExpr
    [ varA @"qvarsym"
        <:> semAct \(qvarsym :* HNil) ->
            qvarsym
    , tokA @"`" <^> varA @"qvarid" <^> tokA @"`"
        <:> semAct \(_ :* _ :* qvarid :* _ :* _ :* HNil) ->
            qvarid
    ]

rConOp :: RuleExpr Id
rConOp = ruleExpr
    [ varA @"consym"
        <:> semAct \(consym :* HNil) ->
            consym
    , tokA @"`" <^> varA @"conid" <^> tokA @"`"
        <:> semAct \(_ :* _ :* conid :* _ :* _ :* HNil) ->
            conid
    ]

rQconOp :: RuleExpr QualifiedId
rQconOp = ruleExpr
    [ varA @"gconsym"
        <:> semAct \(qconsym :* HNil) ->
            qconsym
    , tokA @"`" <^> varA @"qconid" <^> tokA @"`"
        <:> semAct \(_ :* _ :* qconid :* _ :* _ :* HNil) ->
            qconid
    ]

rOp :: RuleExpr Id
rOp = ruleExpr
    [ varA @"varop"
        <:> semAct \(varop :* HNil) ->
            varop
    , varA @"conop"
        <:> semAct \(conop :* HNil) ->
            conop
    ]

rQop :: RuleExpr QualifiedId
rQop = ruleExpr
    [ varA @"qvarop"
        <:> semAct \(qvarop :* HNil) ->
            qvarop
    , varA @"qconop"
        <:> semAct \(qconop :* HNil) ->
            qconop
    ]

rGconsym :: RuleExpr QualifiedId
rGconsym = ruleExpr
    [ tokA @":"
        <:> semAct \(_ :* _ :* HNil) ->
            [||nonQualifiedId (mkId ":")||]
    , varA @"qconsym"
        <:> semAct \(qconsym :* HNil) ->
            qconsym
    ]

rTyvar :: RuleExpr Id
rTyvar = ruleExpr
    [ varA @"varid"
        <:> semAct \(varid :* HNil) ->
            varid
    ]

rTycon :: RuleExpr Id
rTycon = ruleExpr
    [ varA @"conid"
        <:> semAct \(conid :* HNil) ->
            conid
    ]

rQtycon :: RuleExpr QualifiedId
rQtycon = ruleExpr
    [ varA @"qconid"
        <:> semAct \(qconid :* HNil) ->
            qconid
    ]

rVarId :: RuleExpr Id
rVarId = ruleExpr
    [ varA @"qvarid"
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
    [ varA @"qvarsym"
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
    [ varA @"qconid"
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
    [ varA @"qconsym"
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
    [ tokA @"qvarid"
        <:> semAct \(_ :* qvarid :* HNil) ->
            [||case $$(qvarid) of
                TokQualifiedVarId modid varid ->
                    QualifiedId (coerce modid) (Id varid)
                _ ->
                    error "unreachable: expect qvarid"
            ||]
    ]

rQvarSym :: RuleExpr QualifiedId
rQvarSym = ruleExpr
    [ tokA @"qvarsym"
        <:> semAct \(_ :* qvarsym :* HNil) ->
            [||case $$(qvarsym) of
                TokQualifiedVarSym modid varsym ->
                    QualifiedId (coerce modid) (Id varsym)
                _ ->
                    error "unreachable: expect qvarsym"
            ||]
    ]

rQconId :: RuleExpr QualifiedId
rQconId = ruleExpr
    [ tokA @"qconid"
        <:> semAct \(_ :* qconid :* HNil) ->
            [||case $$(qconid) of
                TokQualifiedConId modid conid ->
                    QualifiedId (coerce modid) (Id conid)
                _ ->
                    error "unreachable: expect qconid"
            ||]
    ]

rQconSym :: RuleExpr QualifiedId
rQconSym = ruleExpr
    [ tokA @"qconsym"
        <:> semAct \(_ :* qconsym :* HNil) ->
            [||case $$(qconsym) of
                TokQualifiedConSym modid consym ->
                    QualifiedId (coerce modid) (Id consym)
                _ ->
                    error "unreachable: expect qconsym"
            ||]
    ]

rModId :: RuleExpr QualifiedId
rModId = ruleExpr
    [ varA @"qconid"
        <:> semAct \(qconid :* HNil) ->
            qconid
    ]

rIdExport :: RuleExpr ()
rIdExport = ruleExpr
    [ altId "export"
    ]

rIdHiding :: RuleExpr ()
rIdHiding = ruleExpr
    [ altId "hiding"
    ]

rIdAs :: RuleExpr ()
rIdAs = ruleExpr
    [ altId "as"
    ]

rIdQualified :: RuleExpr ()
rIdQualified = ruleExpr
    [ altId "qualified"
    ]

rSymExclamation :: RuleExpr ()
rSymExclamation = ruleExpr
    [ varA @"varsym"
        <:> semActM \(varsym :* HNil) ->
            [||case $$(varsym) of
                Id txt | txt == Text.pack "!" ->
                    pure ()
                _ ->
                    failAction
            ||]
    ]

rLiteral :: RuleExpr Lit
rLiteral = ruleExpr
    [ varA @"integer"
        <:> semAct \(integer :* HNil) ->
            [||LitInteger $$(integer)||]
    , varA @"float"
        <:> semAct \(float :* HNil) ->
            [||LitFloat $$(float)||]
    , varA @"string"
        <:> semAct \(string :* HNil) ->
            [||LitString $$(string)||]
    , varA @"char"
        <:> semAct \(char :* HNil) ->
            [||LitChar $$(char)||]
    ]

rInteger :: RuleExpr Integer
rInteger = ruleExpr
    [ tokA @"integer"
        <:> semAct \(_ :* integer :* HNil) ->
            [||case $$(integer) of
                TokLitInteger txt ->
                    read (Text.unpack txt) :: Integer
                _ ->
                    error "unreachable: expect integer literal"
            ||]
    ]

rFloat :: RuleExpr Rational
rFloat = ruleExpr
    [ tokA @"float"
        <:> semAct \(_ :* float :* HNil) ->
            [||case $$(float) of
                TokLitFloat txt ->
                    case Numeric.readFloat (Text.unpack txt) :: [(Rational, String)] of
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
    [ tokA @"string"
        <:> semAct \(_ :* string :* HNil) ->
            [||case $$(string) of
                TokLitString txt ->
                    read (Text.unpack txt) :: String
                _ ->
                    error "unreachable: expect string literal"
            ||]
    ]

rChar :: RuleExpr Char
rChar = ruleExpr
    [ tokA @"char"
        <:> semAct \(_ :* char :* HNil) ->
            [||case $$(char) of
                TokLitChar txt ->
                    read (Text.unpack txt) :: Char
                _ ->
                    error "unreachable: expect char literal"
            ||]
    ]

rExpBo :: RuleExpr ()
rExpBo = ruleExpr
    [ tokA @"{"
        <:> semActM \(_ :* _ :* HNil) ->
            [||modifyAction \l -> 0:l||]
    ]

rExpBc :: RuleExpr ()
rExpBc = ruleExpr
    [ tokA @"}"
        <:> semActM \(_ :* _ :* HNil) ->
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
    [ tokA @"{n}"
        <:> semActM \(_ :* expB :* HNil) ->
            [||do
                let n = case $$(expB) of
                        TokVirtExpBrace x ->
                            x
                        _ ->
                            error "unreachable: expect virtual brace start point"
                l <- getAction
                case l of
                    m:_
                        | n > m ->
                            modifyAction \l' -> n:l'
                        | otherwise ->
                            failAction
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
                    _ ->
                        failAction
            ||]
    ]

rSemis0 :: RuleExpr ()
rSemis0 = ruleExpr
    [ varA @"semi" <^> varA @"semi*"
        <:> semAct \(_ :* _ :* HNil) ->
            [||()||]
    , eps
        $ semAct \HNil ->
            [||()||]
    ]

rSemis1 :: RuleExpr ()
rSemis1 = ruleExpr
    [ varA @"semi" <^> varA @"semi*"
        <:> semAct \(_ :* _ :* HNil) ->
            [||()||]
    ]

rSemi :: RuleExpr ()
rSemi = ruleExpr
    [ tokA @";"
        <:> semAct \(_ :* _ :* HNil) ->
            [||()||]
    , tokA @"<n>"
        <:> semActM \(_ :* nl :* HNil) ->
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
                    _ ->
                        failAction
            ||]
    ]

rSkip :: RuleExpr ()
rSkip = ruleExpr
    [ Ptera.tokA @"<n>"
        <:> semActM \(nl :* HNil) ->
            [||do
                let n = case $$(nl) of
                        TokVirtNewline x ->
                            x
                        _ ->
                            error "unreachable: expect newline"
                l <- getAction
                case l of
                    m:_ | m < n ->
                        pure ()
                    [] ->
                        pure ()
                    _ ->
                        failAction
            ||]
    , eps
        $ semAct \HNil ->
            [||()||]
    ]

tokA :: forall t.
    Ptera.TokensMember Tokens t => Ptera.Expr Rules Tokens Token '[(), Token]
tokA = varA @"skip" <^> Ptera.tokA @t

altId :: String -> Alt ()
altId idName = varA @"varid"
    <:> semActM \(varid :* HNil) ->
        [||case $$(varid) of
            Id bs | bs == Text.pack idName ->
                pure ()
            _ ->
                failAction
        ||]

seqToList :: Seq a -> [a]
seqToList = toList

seqFoldl' :: (b -> a -> b) -> b -> Seq a -> b
seqFoldl' f acc xs = foldl' f acc xs
