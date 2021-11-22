{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Parser.Rules where

import           Data.Proxy                        (Proxy (..))
import           Language.Parser.Ptera             hiding (Grammar, RuleExpr)
import qualified Language.Parser.Ptera             as Ptera
import           Language.Parser.Ptera.Data.HList  (HList (..))
import qualified Language.Parser.Ptera.Data.Record as Record
import           Types


grammar :: Ptera.Grammar ParsePoints Rules Token
grammar = fixGrammar $ Record.fromFields Proxy $
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
type RuleExpr = Ptera.RuleExpr Rules Token

instance GrammarToken Token where
    data Terminal Token
        = TPlus
        | TMulti
        | TParenOpen
        | TParenClose
        | TLitInteger
        | TIdentifier
        deriving (Eq, Show, Enum)

    tokenToTerminal token = case token of
        TokPlus{}       -> TPlus
        TokMulti{}      -> TMulti
        TokParenOpen{}  -> TParenOpen
        TokParenClose{} -> TParenClose
        TokLitInteger{} -> TLitInteger
        TokIdentifier{} -> TIdentifier


rExpr :: RuleExpr Ast
rExpr = ruleExpr
    [ alt $ varA @"sum" <:> SemAct \(e :* HNil) -> e
    ]

rSum :: RuleExpr Ast
rSum = ruleExpr
    [ alt $ varA @"product" <^> tok TPlus <^> varA @"sum"
        <:> SemAct \(e1 :* _ :* e2 :* HNil) -> Sum e1 e2
    , alt $ varA @"product"
        <:> SemAct \(e :* HNil) -> e
    ]

rProduct :: RuleExpr Ast
rProduct = ruleExpr
    [ alt $ varA @"value" <^> tok TMulti <^> varA @"product"
        <:> SemAct \(e1 :* _ :* e2 :* HNil) -> Product e1 e2
    , alt $ varA @"value"
        <:> SemAct \(e :* HNil) -> e
    ]

rValue :: RuleExpr Ast
rValue = ruleExpr
    [ alt $ tok TParenOpen <^> varA @"expr" <^> tok TParenClose
        <:> SemAct \(_ :* e :* _ :* HNil) -> e
    , alt $ tok TLitInteger <:> SemAct \(e :* HNil) -> case e of
        TokLitInteger i -> Value i
        _               -> error "unreachable: expected integer token"
    , alt $ tok TIdentifier <:> SemAct \(e :* HNil) -> case e of
        TokIdentifier v -> Var v
        _               -> error "unreachable: expected identifier token"
    ]
