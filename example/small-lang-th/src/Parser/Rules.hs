{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Parser.Rules where

import qualified Language.Haskell.TH      as TH
import           Language.Parser.Ptera.TH hiding (RuleExpr)
import qualified Language.Parser.Ptera.TH as Ptera
import           Types


$(Ptera.genGrammarToken (TH.mkName "Tokens") [t|Token|]
    [ ("+", [p|TokPlus{}|])
    , ("*", [p|TokMulti{}|])
    , ("(", [p|TokParenOpen{}|])
    , (")", [p|TokParenClose{}|])
    , ("int", [p|TokLitInteger{}|])
    , ("id", [p|TokIdentifier{}|])
    , ("EOS", [p|TokEndOfInput{}|])
    ])

$(Ptera.genRules
    (TH.mkName "RuleDefs")
    (GenRulesTypes
        { genRulesCtxTy = [t|()|]
        , genRulesTokensTy = [t|Tokens|]
        , genRulesTokenTy = [t|Token|]
        })
    [ (TH.mkName "rexpreos", "expr EOS", [t|Ast|])
    , (TH.mkName "rexpr", "expr", [t|Ast|])
    , (TH.mkName "rsum", "sum", [t|Ast|])
    , (TH.mkName "rproduct", "product", [t|Ast|])
    , (TH.mkName "rvalue", "value", [t|Ast|])
    ]
    )

$(Ptera.genParsePoints
    (TH.mkName "ParsePoints")
    (TH.mkName "RuleDefs")
    [ "expr EOS"
    ]
    )

grammar :: Grammar RuleDefs Tokens Token ParsePoints
grammar = fixGrammar $ RuleDefs
    { rexpreos = rExprEos
    , rexpr = rExpr
    , rsum = rSum
    , rproduct = rProduct
    , rvalue = rValue
    }

type RuleExpr = Ptera.RuleExpr RuleDefs Tokens Token


rExprEos :: RuleExpr Ast
rExprEos = ruleExpr
    [ varA @"expr" <^> tokA @"EOS"
        <:> \(e :* _ :* HNil) -> e
    ]

rExpr :: RuleExpr Ast
rExpr = ruleExpr
    [ varA @"sum"
        <:> \(e :* HNil) -> e
    ]

rSum :: RuleExpr Ast
rSum = ruleExpr
    [ varA @"product" <^> tokA @"+" <^> varA @"sum"
        <:> \(e1 :* _ :* e2 :* HNil) -> [|| Sum $$(e1) $$(e2) ||]
    , varA @"product"
        <:> \(e :* HNil) -> e
    ]

rProduct :: RuleExpr Ast
rProduct = ruleExpr
    [ varA @"value" <^> tokA @"*" <^> varA @"product"
        <:> \(e1 :* _ :* e2 :* HNil) -> [|| Product $$(e1) $$(e2) ||]
    , varA @"value"
        <:> \(e :* HNil) -> e
    ]

rValue :: RuleExpr Ast
rValue = ruleExpr
    [ tokA @"(" <^> varA @"expr" <^> tokA @")"
        <:> \(_ :* e :* _ :* HNil) -> e
    , tokA @"int" <:> \(e :* HNil) ->
        [|| case $$(e) of
            TokLitInteger i -> Value i
            _               -> error "unreachable: expected integer token"
        ||]
    , tokA @"id" <:> \(e :* HNil) ->
        [|| case $$(e) of
            TokIdentifier v -> Var v
            _               -> error "unreachable: expected identifier token"
        ||]
    ]
