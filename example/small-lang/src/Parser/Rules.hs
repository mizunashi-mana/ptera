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

import           Data.Proxy                       (Proxy (..))
import           Language.Parser.Ptera            hiding (RuleExpr, Rules)
import qualified Language.Parser.Ptera            as Ptera
import           Language.Parser.Ptera.Data.HEnum (henumA)
import qualified Type.Membership.Internal         as MembershipInternal
import           Types


grammar :: Grammar Rules Tokens Token ParsePoints
grammar = fixGrammar $ Rules
    { rexpreos = rExprEos
    , rexpr = rExpr
    , rsum = rSum
    , rproduct = rProduct
    , rvalue = rValue
    }

data Tokens
type instance TokensTag Tokens =
    '[
        "+", "*",
        "(", ")",
        "int",
        "id",
        "EOS"
    ]

type ParsePoints = '[ "expr EOS" ]
data Rules = Rules
    { rexpreos :: RuleExpr Ast
    , rexpr    :: RuleExpr Ast
    , rsum     :: RuleExpr Ast
    , rproduct :: RuleExpr Ast
    , rvalue   :: RuleExpr Ast
    }

type instance RulesTag Rules =
    '[
      "expr EOS"
    , "expr"
    , "sum"
    , "product"
    , "value"
    ]

instance Ptera.Rules Rules where
    generateRules
        = Ptera.HFCons Ptera.DictF
        $ Ptera.HFCons Ptera.DictF
        $ Ptera.HFCons Ptera.DictF
        $ Ptera.HFCons Ptera.DictF
        $ Ptera.HFCons Ptera.DictF
        $ Ptera.HFNil

instance Ptera.MemberInitials Rules ParsePoints where
    memberInitials
        = Ptera.HFCons Ptera.DictF
        $ Ptera.HFNil

instance TokensMember Tokens "+" where
    tokensMembership _ = MembershipInternal.membership

instance TokensMember Tokens "*" where
    tokensMembership _ = MembershipInternal.membership

instance TokensMember Tokens "(" where
    tokensMembership _ = MembershipInternal.membership

instance TokensMember Tokens ")" where
    tokensMembership _ = MembershipInternal.membership

instance TokensMember Tokens "int" where
    tokensMembership _ = MembershipInternal.membership

instance TokensMember Tokens "id" where
    tokensMembership _ = MembershipInternal.membership

instance TokensMember Tokens "EOS" where
    tokensMembership _ = MembershipInternal.membership

instance GrammarToken Tokens Token where
    tokenToTerminal Proxy token = case token of
        TokPlus{}       -> henumA @"+"
        TokMulti{}      -> henumA @"*"
        TokParenOpen{}  -> henumA @"("
        TokParenClose{} -> henumA @")"
        TokLitInteger{} -> henumA @"int"
        TokIdentifier{} -> henumA @"id"
        TokEndOfInput{} -> henumA @"EOS"

instance HasField "expr EOS" Rules (RuleExpr Ast) where
    getField = rexpreos

instance HasField "expr" Rules (RuleExpr Ast) where
    getField = rexpr

instance HasField "sum" Rules (RuleExpr Ast) where
    getField = rsum

instance HasField "product" Rules (RuleExpr Ast) where
    getField = rproduct

instance HasField "value" Rules (RuleExpr Ast) where
    getField = rvalue

instance HasRuleExprField Rules "expr EOS" where
    type RuleExprReturnType Rules "expr EOS" = Ast

instance HasRuleExprField Rules "expr" where
    type RuleExprReturnType Rules "expr" = Ast

instance HasRuleExprField Rules "sum" where
    type RuleExprReturnType Rules "sum" = Ast

instance HasRuleExprField Rules "product" where
    type RuleExprReturnType Rules "product" = Ast

instance HasRuleExprField Rules "value" where
    type RuleExprReturnType Rules "value" = Ast

type RuleExpr = Ptera.RuleExpr Rules Tokens Token
type instance RuleExprType Rules = RuleExpr


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
        <:> \(e1 :* _ :* e2 :* HNil) -> Sum e1 e2
    , varA @"product"
        <:> \(e :* HNil) -> e
    ]

rProduct :: RuleExpr Ast
rProduct = ruleExpr
    [ varA @"value" <^> tokA @"*" <^> varA @"product"
        <:> \(e1 :* _ :* e2 :* HNil) -> Product e1 e2
    , varA @"value"
        <:> \(e :* HNil) -> e
    ]

rValue :: RuleExpr Ast
rValue = ruleExpr
    [ tokA @"(" <^> varA @"expr" <^> tokA @")"
        <:> \(_ :* e :* _ :* HNil) -> e
    , tokA @"int" <:> \(e :* HNil) -> case e of
        TokLitInteger i -> Value i
        _               -> error "unreachable: expected integer token"
    , tokA @"id" <:> \(e :* HNil) -> case e of
        TokIdentifier v -> Var v
        _               -> error "unreachable: expected identifier token"
    ]
