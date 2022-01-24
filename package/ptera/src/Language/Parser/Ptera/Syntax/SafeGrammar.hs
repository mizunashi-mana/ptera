{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Language.Parser.Ptera.Syntax.SafeGrammar (
    T,

    Grammar (..),
    TokensTag,
    RulesTag,
    RuleExprType,
    GrammarToken (..),
    fixGrammar,

    StartPoint,
    Terminal,
    NonTerminal,
    HasRuleExprField (..),
    MemberInitials,
    Rules,
    genStartPoint,

    RuleExpr (..),
    Alt (..),
    Expr (..),
    ruleExpr,
    (<^>),
    (<:>),
    eps,
    var,
    varA,
    tok,
    TokensMember (..),
    tokA,
) where

import           Language.Parser.Ptera.Prelude

import qualified Data.HashMap.Strict                  as HashMap
import qualified Language.Parser.Ptera.Data.HEnum     as HEnum
import qualified Language.Parser.Ptera.Syntax.Grammar as SyntaxGrammar
import           Prelude                              (String)
import qualified Type.Membership                      as Membership
import qualified Type.Membership.HList                      as Membership

type T = Grammar

type Grammar
    :: ([Type] -> Type -> Type) -> Type -> Type -> Type -> [Symbol]
    -> Type
newtype Grammar action rules tokens elem initials = UnsafeGrammar
    {
        unsafeGrammar :: SyntaxGrammar.FixedGrammar
            StartPoint
            NonTerminal
            Terminal
            elem
            StringLit
            (Maybe ())
            action
    }

type family TokensTag (tokens :: Type) :: [Symbol]
type family RulesTag (rules :: Type) :: [Symbol]
type family RuleExprType (rules :: Type) :: Type -> Type

class GrammarToken tokens elem where
    tokenToTerminal :: Proxy tokens -> elem -> HEnum.T (TokensTag tokens)

class
    ( KnownSymbol v
    , HasField v rules ((RuleExprType rules) (RuleExprReturnType rules v))
    ) => HasRuleExprField rules v where
    type RuleExprReturnType rules v :: Type

    nonTerminalName :: Proxy# rules -> Proxy# v -> String
    nonTerminalName _ p# = symbolVal' p#

type GrammarMForFixGrammar elem action = SyntaxGrammar.GrammarT
    StartPoint
    NonTerminal
    Terminal
    elem
    StringLit
    (Maybe ())
    action
    Identity

fixGrammar
    :: forall initials action rules tokens elem
    .  MemberInitials rules initials
    => Rules rules => RuleExprType rules ~ RuleExpr action rules tokens elem
    => rules -> Grammar action rules tokens elem initials
fixGrammar ruleDefs = UnsafeGrammar do
    runIdentity do
        SyntaxGrammar.fixGrammarT do
            Membership.henumerateFor
                do Proxy @(HasRuleExprField rules)
                do Proxy @initials
                do fixInitial
                do pure ()
            Membership.henumerateFor
                do Proxy @(HasRuleExprField rules)
                do Proxy @(RulesTag rules)
                do fixRule
                do pure ()
    where
        fixInitial :: forall v. HasRuleExprField rules v
            => Membership.Membership initials v
            -> GrammarMForFixGrammar elem action ()
            -> GrammarMForFixGrammar elem action ()
        fixInitial m g = g >> do
            let sn = genStartPoint m
            let vn = getNewV
                    do symbolVal' do proxy# @v
            SyntaxGrammar.initialT sn vn

        fixRule :: forall v. HasRuleExprField rules v
            => Membership.Membership (RulesTag rules) v
            -> GrammarMForFixGrammar elem action ()
            -> GrammarMForFixGrammar elem action ()
        fixRule _ g = g >> do
            let vn = getNewV
                    do symbolVal' do proxy# @v
                d = nonTerminalName
                        do proxy# @rules
                        do proxy# @v
            SyntaxGrammar.ruleT vn d do
                fixRuleExpr do getField @v ruleDefs

        fixRuleExpr :: RuleExpr action rules tokens elem a
            -> SyntaxGrammar.RuleExpr NonTerminal Terminal elem (Maybe ()) action
        fixRuleExpr = \case
            RuleExpr alts -> SyntaxGrammar.RuleExpr
                [ fixAlt origAlt | origAlt <- alts ]

        fixAlt :: Alt action rules tokens elem a
            -> SyntaxGrammar.Alt NonTerminal Terminal elem (Maybe ()) action a
        fixAlt (UnsafeAlt origAlt) = case origAlt of
            SyntaxGrammar.Alt e h act -> SyntaxGrammar.Alt
                do fixExpr e
                do h
                do act

        fixExpr :: SyntaxGrammar.Expr IntermNonTerminal Terminal elem us
            -> SyntaxGrammar.Expr NonTerminal Terminal elem us
        fixExpr = \case
            Membership.HNil ->
                Membership.HNil
            Membership.HCons u1 e2 ->
                Membership.HCons
                    do fixUnit u1
                    do fixExpr e2

        fixUnit :: SyntaxGrammar.Unit IntermNonTerminal Terminal elem u
            -> SyntaxGrammar.Unit NonTerminal Terminal elem u
        fixUnit u = case u of
            SyntaxGrammar.UnitToken t ->
                SyntaxGrammar.UnitToken t
            SyntaxGrammar.UnitVar v ->
                SyntaxGrammar.UnitVar do getNewV v

        rulesTag = genRulesTagMap do proxy# @rules

        getNewV v = case HashMap.lookup v rulesTag of
            Just newV ->
                newV
            Nothing ->
                error "unreachable: rulesTag must include v."

type StartPoint = Int
type Terminal = Int
type NonTerminal = Int
type IntermNonTerminal = String

class Membership.Forall (HasRuleExprField rules) initials
        => MemberInitials rules initials
instance Membership.Forall (HasRuleExprField rules) initials
        => MemberInitials rules initials

class Membership.Forall (HasRuleExprField rules) (RulesTag rules)
        => Rules rules
instance Membership.Forall (HasRuleExprField rules) (RulesTag rules)
        => Rules rules

genStartPoint :: forall initials v. Membership.Membership initials v -> StartPoint
genStartPoint m = Membership.getMemberId m

genRulesTagMap :: forall rules.
    Rules rules => Proxy# rules -> HashMap.HashMap IntermNonTerminal NonTerminal
genRulesTagMap _ = Membership.henumerateFor
    do Proxy @(HasRuleExprField rules)
    do Proxy @(RulesTag rules)
    do go
    do HashMap.empty
    where
        go :: forall v. HasRuleExprField rules v
            => Membership.Membership (RulesTag rules) v
            -> HashMap.HashMap IntermNonTerminal NonTerminal
            -> HashMap.HashMap IntermNonTerminal NonTerminal
        go member m = HashMap.insert
            do symbolVal' do proxy# @v
            do Membership.getMemberId member
            do m

type RuleExpr :: ([Type] -> Type -> Type) -> Type -> Type -> Type -> Type -> Type
newtype RuleExpr action rules tokens elem a = RuleExpr
    { unRuleExpr :: [Alt action rules tokens elem a]
    }

type Alt :: ([Type] -> Type -> Type) -> Type -> Type -> Type -> Type -> Type
newtype Alt action rules tokens elem a = UnsafeAlt
    { unsafeAlt
        :: SyntaxGrammar.Alt IntermNonTerminal Terminal elem (Maybe ()) action a
    }

type Expr :: Type -> Type -> Type -> [Type] -> Type
newtype Expr rules tokens elem us = UnsafeExpr
    { unsafeExpr :: SyntaxGrammar.Expr IntermNonTerminal Terminal elem us
    }

class TokensMember tokens t where
    tokensMembership :: Proxy# '(tokens, t) -> Membership.Membership (TokensTag tokens) t

ruleExpr :: [Alt action rules tokens elem a] -> RuleExpr action rules tokens elem a
ruleExpr alts = RuleExpr alts

(<:>)
    :: Expr rules tokens elem us -> action us a
    -> Alt action rules tokens elem a
UnsafeExpr e <:> act = UnsafeAlt do SyntaxGrammar.Alt e Nothing act

infixl 4 <:>

eps :: action '[] a -> Alt action rules tokens elem a
eps act = UnsafeAlt do SyntaxGrammar.Alt Membership.HNil Nothing act

(<^>)
    :: Expr rules tokens elem us1 -> Expr rules tokens elem us2
    -> Expr rules tokens elem (Concat us1 us2)
UnsafeExpr e1 <^> UnsafeExpr e2 = UnsafeExpr do hconcat e1 e2 where
    hconcat
        :: Membership.HList f xs1 -> Membership.HList f xs2
        -> Membership.HList f (Concat xs1 xs2)
    hconcat hl1 hl2 = case hl1 of
        Membership.HNil ->
            hl2
        Membership.HCons x hl1' ->
            Membership.HCons x do hconcat hl1' hl2

infixr 5 <^>

type family Concat (us1 :: [k]) (us2 :: [k]) :: [k] where
    Concat '[] us2 =
        us2
    Concat (u ': us1) us2 =
        u ': Concat us1 us2

var :: KnownSymbol v => proxy v -> Expr rules tokens elem '[RuleExprReturnType rules v]
var p = UnsafeExpr do Membership.HCons u Membership.HNil where
    u = SyntaxGrammar.UnitVar do symbolVal p

varA :: forall v rules tokens elem.
    KnownSymbol v => Expr rules tokens elem '[RuleExprReturnType rules v]
varA = var do Proxy @v

tok :: Membership.Membership (TokensTag tokens) t -> Expr rules tokens elem '[elem]
tok p = UnsafeExpr do Membership.HCons u Membership.HNil where
    u = SyntaxGrammar.UnitToken
        do HEnum.unsafeHEnum do HEnum.henum p

tokA :: forall t rules tokens elem.
    TokensMember tokens t => Expr rules tokens elem '[elem]
tokA = tok do tokensMembership do proxy# @'(tokens, t)
