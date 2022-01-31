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
    MemberInitials (..),
    Rules (..),
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
import qualified Language.Parser.Ptera.Data.HFList    as HFList
import qualified Language.Parser.Ptera.Syntax.Grammar as SyntaxGrammar
import           Prelude                              (String)
import qualified Type.Membership                      as Membership


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

    nonTerminalName :: rules -> proxy v -> String
    nonTerminalName _ p = symbolVal p

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
            HFList.hforMWithIndex
                memberInitials
                fixInitial
            HFList.hforMWithIndex
                generateRules
                fixRule
    where
        fixInitial
            :: Membership.Membership initials v
            -> HFList.DictF (HasRuleExprField rules) v
            -> GrammarMForFixGrammar elem action ()
        fixInitial m HFList.DictF = do
            let sn = genStartPoint m
            let vn = getNewV do symbolVal m
            SyntaxGrammar.initialT sn vn

        fixRule
            :: forall v
            .  Membership.Membership (RulesTag rules) v
            -> HFList.DictF (HasRuleExprField rules) v
            -> GrammarMForFixGrammar elem action ()
        fixRule m HFList.DictF = do
            let vn = getNewV do symbolVal m
            let d = nonTerminalName ruleDefs m
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
        fixExpr = HFList.hmapWithIndex
            do \_ u1 -> fixUnit u1

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


class MemberInitials rules initials where
    memberInitials :: HFList.T (HFList.DictF (HasRuleExprField rules)) initials

class Rules rules where
    generateRules :: HFList.T (HFList.DictF (HasRuleExprField rules)) (RulesTag rules)


genStartPoint :: forall initials v. Membership.Membership initials v -> StartPoint
genStartPoint m = Membership.getMemberId m

genRulesTagMap :: forall rules.
    Rules rules => Proxy# rules -> HashMap.HashMap IntermNonTerminal NonTerminal
genRulesTagMap _ = HFList.hfoldlWithIndex HashMap.empty go generateRules where
    go :: forall v
        .  HashMap.HashMap IntermNonTerminal NonTerminal
        -> Membership.Membership (RulesTag rules) v
        -> HFList.DictF (HasRuleExprField rules) v
        -> HashMap.HashMap IntermNonTerminal NonTerminal
    go vMap m HFList.DictF = HashMap.insert
        do symbolVal' do proxy# @v
        do Membership.getMemberId m
        do vMap

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

eps :: Expr rules tokens elem '[]
eps = UnsafeExpr HFList.HFNil

(<^>)
    :: Expr rules tokens elem us1 -> Expr rules tokens elem us2
    -> Expr rules tokens elem (HFList.Concat us1 us2)
UnsafeExpr e1 <^> UnsafeExpr e2 = UnsafeExpr do HFList.hconcat e1 e2

infixr 5 <^>

var :: KnownSymbol v => proxy v -> Expr rules tokens elem '[RuleExprReturnType rules v]
var p = UnsafeExpr do HFList.HFCons u HFList.HFNil where
    u = SyntaxGrammar.UnitVar do symbolVal p

varA :: forall v rules tokens elem.
    KnownSymbol v => Expr rules tokens elem '[RuleExprReturnType rules v]
varA = var do Proxy @v

tok :: Membership.Membership (TokensTag tokens) t -> Expr rules tokens elem '[elem]
tok p = UnsafeExpr do HFList.HFCons u HFList.HFNil where
    u = SyntaxGrammar.UnitToken
        do HEnum.unsafeHEnum do HEnum.henum p

tokA :: forall t rules tokens elem.
    TokensMember tokens t => Expr rules tokens elem '[elem]
tokA = tok do tokensMembership do proxy# @'(tokens, t)
