{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Language.Parser.Ptera.Syntax.SafeGrammar (
    T,

    Grammar (..),
    RulesTag,
    RuleExprType,
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
    Unit (..),
    ruleExpr,
    alt,
    (<^>),
    (<:>),
    eps,
    var,
    varA,
    tok,
    tokA,
) where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Data.HEnum     as HEnum
import qualified Language.Parser.Ptera.Syntax.Grammar as SyntaxGrammar
import Prelude (String)
import qualified Data.HashMap.Strict as HashMap
import qualified Type.Membership as Membership
import qualified Type.Membership.Internal as MembershipInternal

type T = Grammar

type Grammar :: ([Type] -> Type -> Type) -> Type -> [t] -> Type -> [n] -> Type
newtype Grammar action rules tokens elem initials = UnsafeGrammar
    {
        unsafeGrammar ::
            SyntaxGrammar.FixedGrammar StartPoint NonTerminal Terminal elem action
    }

type family RulesTag (rules :: Type) :: [Symbol]
type family RuleExprType (rules :: Type) :: Type -> Type

class (KnownSymbol v, HasField v rules ((RuleExprType rules) (RuleExprReturnType rules v))) =>
        HasRuleExprField rules v where
    type RuleExprReturnType rules v :: Type

    nonTerminalName :: Proxy# rules -> Proxy# v -> String
    nonTerminalName _ p# = symbolVal' p#

type GrammarMForFixGrammar elem action =
    SyntaxGrammar.GrammarT StartPoint NonTerminal Terminal elem action Identity

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
            let vn = genNewV
                    do nonTerminalName
                        do proxy# @rules
                        do proxy# @v
            SyntaxGrammar.initialT sn vn

        fixRule :: forall v. HasRuleExprField rules v
            => Membership.Membership (RulesTag rules) v
            -> GrammarMForFixGrammar elem action ()
            -> GrammarMForFixGrammar elem action ()
        fixRule _ g = g >> do
            let vn = genNewV
                    do nonTerminalName
                        do proxy# @rules
                        do proxy# @v
            SyntaxGrammar.ruleT vn do
                fixRuleExpr do getField @v ruleDefs

        fixRuleExpr :: RuleExpr action rules tokens elem a
            -> SyntaxGrammar.RuleExpr NonTerminal Terminal elem action
        fixRuleExpr (UnsafeRuleExpr alts) = SyntaxGrammar.RuleExpr
            [ coerce fixAlt origAlt | origAlt <- alts ]

        fixAlt :: Alt action rules tokens elem a
            -> SyntaxGrammar.Alt NonTerminal Terminal elem action a
        fixAlt (UnsafeAlt origAlt) = case origAlt of
            SyntaxGrammar.Alt e act -> SyntaxGrammar.Alt
                do coerce fixExpr e
                do act

        fixExpr :: Expr rules tokens elem us
            -> SyntaxGrammar.Expr NonTerminal Terminal elem us
        fixExpr (UnsafeExpr e) = case e of
            SyntaxGrammar.Eps ->
                SyntaxGrammar.Eps
            u1 SyntaxGrammar.:^ e2 ->
                coerce fixUnit u1 SyntaxGrammar.:^ coerce fixExpr e2

        fixUnit :: Unit rules tokens elem u
            -> SyntaxGrammar.Unit NonTerminal Terminal elem u
        fixUnit (UnsafeUnit u) = case u of
            SyntaxGrammar.UnitToken t ->
                SyntaxGrammar.UnitToken t
            SyntaxGrammar.UnitVar v ->
                SyntaxGrammar.UnitVar do genNewV v

        rulesTag = genRulesTagMap do proxy# @rules

        genNewV v = case HashMap.lookup v rulesTag of
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

type RuleExpr :: ([Type] -> Type -> Type) -> Type -> [t] -> Type -> Type -> Type
newtype RuleExpr action rules tokens elem a = UnsafeRuleExpr
    {
        unsafeRuleExpr :: [SyntaxGrammar.Alt IntermNonTerminal Terminal elem action a]
    }

type Alt :: ([Type] -> Type -> Type) -> Type -> [t] -> Type -> Type -> Type
newtype Alt action rules tokens elem a = UnsafeAlt
    {
        unsafeAlt :: SyntaxGrammar.Alt IntermNonTerminal Terminal elem action a
    }

type Expr :: Type -> [t] -> Type -> [Type] -> Type
newtype Expr rules tokens elem us = UnsafeExpr
    {
        unsafeExpr :: SyntaxGrammar.Expr IntermNonTerminal Terminal elem us
    }

type Unit :: Type -> [t] -> Type -> Type -> Type
newtype Unit rules tokens elem u = UnsafeUnit
    {
        unsafeUnit :: SyntaxGrammar.Unit IntermNonTerminal Terminal elem u
    }

ruleExpr :: [Alt action rules tokens elem a] -> RuleExpr action rules tokens elem a
ruleExpr alts = UnsafeRuleExpr do coerce alts

alt :: (Expr rules tokens elem us, action us a) -> Alt action rules tokens elem a
alt (UnsafeExpr us, act) = UnsafeAlt do SyntaxGrammar.Alt us act

(<^>) :: Unit rules tokens elem u -> (Expr rules tokens elem us1, action us2 a)
    -> (Expr rules tokens elem (u ': us1), action us2 a)
UnsafeUnit u <^> (UnsafeExpr us, act) = (UnsafeExpr do u SyntaxGrammar.:^ us, act)

infixr 5 <^>

(<:>) :: Unit rules tokens elem u -> action us2 a -> (Expr rules tokens elem '[u], action us2 a)
UnsafeUnit u <:> act = (UnsafeExpr do u SyntaxGrammar.:^ SyntaxGrammar.Eps, act)

infixr 5 <:>

eps :: action '[] a -> Alt action rules tokens elem a
eps act = UnsafeAlt do SyntaxGrammar.Alt SyntaxGrammar.Eps act

var :: forall v rules tokens elem a proxy.
    HasRuleExprField rules v
    => proxy v -> Unit rules tokens elem a
var p = UnsafeUnit do SyntaxGrammar.UnitVar do symbolVal p

varA :: forall v rules tokens elem a.
    HasRuleExprField rules v => Unit rules tokens elem a
varA = var do Proxy @v

tok :: Membership.Membership tokens t -> Unit rules tokens elem elem
tok p = UnsafeUnit
    do SyntaxGrammar.UnitToken
        do HEnum.unsafeHEnum do HEnum.henum p

tokA :: forall t rules tokens elem.
    Membership.Member tokens t => Unit rules tokens elem elem
tokA = tok do MembershipInternal.membership @tokens @t
