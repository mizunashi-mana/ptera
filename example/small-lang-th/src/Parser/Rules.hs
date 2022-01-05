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
import           Language.Parser.Ptera.TH          hiding (RuleExpr, Rules)
import qualified Language.Parser.Ptera.TH          as Ptera
import           Types
import qualified Language.Haskell.TH as TH


$(Ptera.genGrammarToken (TH.mkName "Tokens") [t|Token|]
    [ ("+", [p|TokPlus{}|])
    , ("*", [p|TokMulti{}|])
    , ("(", [p|TokParenOpen{}|])
    , (")", [p|TokParenClose{}|])
    , ("int", [p|TokLitInteger{}|])
    , ("id", [p|TokIdentifier{}|])
    ])

$(Ptera.genRules
    do TH.mkName "Rules"
    do GenRulesTypes
        { genRulesCtxTy = [t|()|]
        , genRulesTokensTy = [t|Tokens|]
        , genRulesTokenTy = [t|Token|]
        }
    [ (TH.mkName "rexpr", "expr", [t|Ast|])
    , (TH.mkName "rsum", "sum", [t|Ast|])
    , (TH.mkName "rproduct", "product", [t|Ast|])
    , (TH.mkName "rvalue", "value", [t|Ast|])
    ]
    )

grammar :: Grammar Rules Tokens Token ParsePoints
grammar = fixGrammar $ Rules
    { rexpr = rExpr
    , rsum = rSum
    , rproduct = rProduct
    , rvalue = rValue
    }

type ParsePoints = '[ "expr" ]

type RuleExpr = Ptera.RuleExpr Rules Tokens Token


rExpr :: RuleExpr Ast
rExpr = ruleExpr
    [ alt $ varA @"sum" <:> semAct \(e :* HNil) -> e
    ]

rSum :: RuleExpr Ast
rSum = ruleExpr
    [ alt $ varA @"product" <^> tokA @"+" <^> varA @"sum"
        <:> semAct \(e1 :* _ :* e2 :* HNil) -> [|| Sum $$(e1) $$(e2) ||]
    , alt $ varA @"product"
        <:> semAct \(e :* HNil) -> e
    ]

rProduct :: RuleExpr Ast
rProduct = ruleExpr
    [ alt $ varA @"value" <^> tokA @"*" <^> varA @"product"
        <:> semAct \(e1 :* _ :* e2 :* HNil) -> [|| Product $$(e1) $$(e2) ||]
    , alt $ varA @"value"
        <:> semAct \(e :* HNil) -> e
    ]

rValue :: RuleExpr Ast
rValue = ruleExpr
    [ alt $ tokA @"(" <^> varA @"expr" <^> tokA @")"
        <:> semAct \(_ :* e :* _ :* HNil) -> e
    , alt $ tokA @"int" <:> semAct \(e :* HNil) ->
        [|| case $$(e) of
            TokLitInteger i -> Value i
            _               -> error "unreachable: expected integer token"
        ||]
    , alt $ tokA @"id" <:> semAct \(e :* HNil) ->
        [|| case $$(e) of
            TokIdentifier v -> Var v
            _               -> error "unreachable: expected identifier token"
        ||]
    ]
