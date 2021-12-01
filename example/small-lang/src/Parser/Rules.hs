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
import qualified Language.Parser.Ptera.Data.Record as Record
import           Types


grammar :: Grammar ParsePoints Rules Tokens Token
grammar = fixGrammar $ Record.fromFieldsA $
    Record.field #expr rExpr :*
    Record.field #sum rSum :*
    Record.field #product rProduct :*
    Record.field #value rValue :*
    HNil

type ParsePoints = '[ "expr" ]
type Rules =
    '[
        '("expr", Ast),
        '("sum", Ast),
        '("product", Ast),
        '("value", Ast)
    ]
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
    [ alt $ varA @"sum" <:> SemAct \(e :* HNil) -> e
    ]

rSum :: RuleExpr Ast
rSum = ruleExpr
    [ alt $ varA @"product" <^> tokA @"+" <^> varA @"sum"
        <:> SemAct \(e1 :* _ :* e2 :* HNil) -> Sum e1 e2
    , alt $ varA @"product"
        <:> SemAct \(e :* HNil) -> e
    ]

rProduct :: RuleExpr Ast
rProduct = ruleExpr
    [ alt $ varA @"value" <^> tokA @"*" <^> varA @"product"
        <:> SemAct \(e1 :* _ :* e2 :* HNil) -> Product e1 e2
    , alt $ varA @"value"
        <:> SemAct \(e :* HNil) -> e
    ]

rValue :: RuleExpr Ast
rValue = ruleExpr
    [ alt $ tokA @"(" <^> varA @"expr" <^> tokA @")"
        <:> SemAct \(_ :* e :* _ :* HNil) -> e
    , alt $ tokA @"int" <:> SemAct \(e :* HNil) -> case e of
        TokLitInteger i -> Value i
        _               -> error "unreachable: expected integer token"
    , alt $ tokA @"id" <:> SemAct \(e :* HNil) -> case e of
        TokIdentifier v -> Var v
        _               -> error "unreachable: expected identifier token"
    ]
