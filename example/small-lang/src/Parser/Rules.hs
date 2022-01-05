{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Parser.Rules where

import           Data.Proxy                        (Proxy (..))
import           Language.Parser.Ptera             hiding (RuleExpr, Rules)
import qualified Language.Parser.Ptera             as Ptera
import           Language.Parser.Ptera.Data.HEnum  (henumA)
import           Language.Parser.Ptera.Data.HList  (HList (..))
import           Types


grammar :: Grammar Rules Tokens Token ParsePoints
grammar = fixGrammar $ Rules
    { rexpr = rExpr
    , rsum = rSum
    , rproduct = rProduct
    , rvalue = rValue
    }

type ParsePoints = '[ "expr" ]
data Rules = Rules
    { rexpr :: RuleExpr Ast
    , rsum :: RuleExpr Ast
    , rproduct :: RuleExpr Ast
    , rvalue :: RuleExpr Ast
    }

type instance RulesTag Rules =
    '[
      "expr"
    , "sum"
    , "product"
    , "value"
    ]

instance HasField "expr" Rules (RuleExpr Ast) where
    getField = rexpr

instance HasField "sum" Rules (RuleExpr Ast) where
    getField = rsum

instance HasField "product" Rules (RuleExpr Ast) where
    getField = rproduct

instance HasField "value" Rules (RuleExpr Ast) where
    getField = rvalue

instance HasRuleExprField SemAct Rules Tokens Token "expr" where
    type RuleExprReturnType SemAct Rules Tokens Token "expr" = Ast

instance HasRuleExprField SemAct Rules Tokens Token "sum" where
    type RuleExprReturnType SemAct Rules Tokens Token "sum" = Ast

instance HasRuleExprField SemAct Rules Tokens Token "product" where
    type RuleExprReturnType SemAct Rules Tokens Token "product" = Ast

instance HasRuleExprField SemAct Rules Tokens Token "value" where
    type RuleExprReturnType SemAct Rules Tokens Token "value" = Ast

type Tokens =
    '[
        "+", "*",
        "(", ")",
        "int",
        "id"
    ]
type RuleExpr = Ptera.RuleExpr Rules Tokens Token

instance GrammarToken Token Tokens where
    tokenToTerminal Proxy token = case token of
        TokPlus{}       -> henumA @"+"
        TokMulti{}      -> henumA @"*"
        TokParenOpen{}  -> henumA @"("
        TokParenClose{} -> henumA @")"
        TokLitInteger{} -> henumA @"int"
        TokIdentifier{} -> henumA @"id"


rExpr :: RuleExpr Ast
rExpr = ruleExpr
    [ alt $ varA @"sum" <:> semAct \(e :* HNil) -> e
    ]

rSum :: RuleExpr Ast
rSum = ruleExpr
    [ alt $ varA @"product" <^> tokA @"+" <^> varA @"sum"
        <:> semAct \(e1 :* _ :* e2 :* HNil) -> Sum e1 e2
    , alt $ varA @"product"
        <:> semAct \(e :* HNil) -> e
    ]

rProduct :: RuleExpr Ast
rProduct = ruleExpr
    [ alt $ varA @"value" <^> tokA @"*" <^> varA @"product"
        <:> semAct \(e1 :* _ :* e2 :* HNil) -> Product e1 e2
    , alt $ varA @"value"
        <:> semAct \(e :* HNil) -> e
    ]

rValue :: RuleExpr Ast
rValue = ruleExpr
    [ alt $ tokA @"(" <^> varA @"expr" <^> tokA @")"
        <:> semAct \(_ :* e :* _ :* HNil) -> e
    , alt $ tokA @"int" <:> semAct \(e :* HNil) -> case e of
        TokLitInteger i -> Value i
        _               -> error "unreachable: expected integer token"
    , alt $ tokA @"id" <:> semAct \(e :* HNil) -> case e of
        TokIdentifier v -> Var v
        _               -> error "unreachable: expected identifier token"
    ]
