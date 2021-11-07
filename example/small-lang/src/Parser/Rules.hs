{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Parser.Rules where

import           Data.Proxy                       (Proxy (..))
import           Language.Parser.Ptera            hiding (Grammar, Rule)
import qualified Language.Parser.Ptera            as Ptera
import           Language.Parser.Ptera.Data.HList (HList (..))


type Grammar = Ptera.Grammar ParsePoints NonTerminal Terminal Token
type ParsePoints =
    '[
        '("expr", Ast)
    ]
type Rule a = Grammar (Ptera.Rule NonTerminal a)

data NonTerminal
    = NtExpr
    | NtSum
    | NtProduct
    | NtValue
    deriving (Eq, Show, Enum)

data Terminal
    = TPlus
    | TMulti
    | TParenOpen
    | TParenClose
    | TLitInteger
    | TIdentifier
    deriving (Eq, Show, Enum)

data Token
    = TokPlus
    | TokMulti
    | TokParenOpen
    | TokParenClose
    | TokLitInteger Integer
    | TokIdentifier String
    deriving (Eq, Show)

data Ast
    = Sum Ast Ast
    | Product Ast Ast
    | Var String
    | Value Integer
    deriving (Eq, Show)


grammar :: Grammar ()
grammar = initial (Proxy :: Proxy "expr") rExpr

rExpr :: Rule Ast
rExpr = rule NtExpr
    [ alt $ var rSum <:> \(e :* HNil) -> e
    ]

rSum :: Rule Ast
rSum = rule NtSum
    [ alt $ var rProduct <^> tok TPlus <^> var rSum
        <:> \(e1 :* _ :* e2 :* HNil) -> Sum e1 e2
    , alt $ var rProduct
        <:> \(e :* HNil) -> e
    ]

rProduct :: Rule Ast
rProduct = rule NtProduct
    [ alt $ var rValue <^> tok TMulti <^> var rProduct
        <:> \(e1 :* _ :* e2 :* HNil) -> Product e1 e2
    , alt $ var rValue
        <:> \(e :* HNil) -> e
    ]

rValue :: Rule Ast
rValue = rule NtValue
    [ alt $ tok TParenOpen <^> var rExpr <^> tok TParenClose
        <:> \(_ :* e :* _ :* HNil) -> e
    , alt $ tok TLitInteger <:> \(e :* HNil) -> case e of
        TokLitInteger i -> Value i
        _               -> error "unreachable: expected integer token"
    , alt $ tok TIdentifier <:> \(e :* HNil) -> case e of
        TokIdentifier v -> Var v
        _               -> error "unreachable: expected identifier token"
    ]
