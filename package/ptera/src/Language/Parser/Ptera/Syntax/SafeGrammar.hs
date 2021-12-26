{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Parser.Ptera.Syntax.SafeGrammar where

import           Language.Parser.Ptera.Prelude

import qualified Data.Array                           as Array
import qualified Language.Parser.Ptera.Data.HEnum     as HEnum
import qualified Language.Parser.Ptera.Data.Member    as Member
import qualified Language.Parser.Ptera.Data.Record    as Record
import qualified Language.Parser.Ptera.Data.TypeOps   as TypeOps
import qualified Language.Parser.Ptera.Syntax.Grammar as SyntaxGrammar
import qualified Unsafe.Coerce                        as Unsafe

type T = Grammar

fixGrammar :: forall action vars rules tokens elem.
    MemberInitials rules vars
    => Rules action rules tokens elem -> Grammar action vars rules tokens elem
fixGrammar (Record.UnsafeRecord arr) = UnsafeGrammar do
    runIdentity do
        SyntaxGrammar.fixGrammarT do
            unsafeInitials
                do proxy# :: Proxy# rules
                do proxy# :: Proxy# vars

            forM_ do Array.assocs arr
                \(p, e) ->
                    let RuleExpr alts = Unsafe.unsafeCoerce e
                    in SyntaxGrammar.ruleT p do SyntaxGrammar.RuleExpr alts

type Grammar :: ([Type] -> Type -> Type) -> [n] -> [(n, Type)] -> [t] -> Type -> Type
newtype Grammar action vars rules tokens elem = UnsafeGrammar
    {
        unsafeGrammar ::
            SyntaxGrammar.FixedGrammar StartPoint NonTerminal Terminal elem action
    }

type StartPoint = Int
type Terminal = Int
type NonTerminal = Int

type MemberInitials rules vars = MemberInitialsGo rules vars vars

unsafeInitials :: MemberInitials rules vars
    => Proxy# rules -> Proxy# vars
    -> SyntaxGrammar.GrammarT StartPoint NonTerminal Terminal elem action Identity ()
unsafeInitials hp# sp# = unsafeInitialsGo hp# sp# sp#

class MemberInitialsGo (rules :: [(n, Type)]) (vars1 :: [n]) (vars2 :: [n]) where
    unsafeInitialsGo :: Proxy# rules -> Proxy# vars1 -> Proxy# vars2
        -> SyntaxGrammar.GrammarT StartPoint NonTerminal Terminal elem action Identity ()

instance MemberInitialsGo rules vars1 '[] where
    unsafeInitialsGo _ _ _ = pure ()

instance (Member.T v vars1, Record.RecordMember v rules, MemberInitialsGo rules vars1 vars2)
        => MemberInitialsGo rules vars1 (v ': vars2) where
    unsafeInitialsGo hp# sp# _ = do
        SyntaxGrammar.initialT sn vn
        unsafeInitialsGo hp# sp# do proxy# :: Proxy# vars2
        where
            sn = Member.position
                do proxy# :: Proxy# v
                sp#
            vn = Record.unsafePosition
                do proxy# :: Proxy# v
                hp#


type Rules action rules tokens elem = Record.T (RulesTag action rules tokens elem)

type RulesTag :: ([Type] -> Type -> Type) -> [(n, Type)] -> [t] -> Type -> [(n, Type)]
type family RulesTag action rules tokens elem where
    RulesTag action rules tokens elem = TypeOps.MapMapSnd (RuleExpr action rules tokens elem) rules

type RuleExpr :: ([Type] -> Type -> Type) -> [(n, Type)] -> [t] -> Type -> Type -> Type
newtype RuleExpr action rules tokens elem a = RuleExpr
    {
        unsafeRuleExpr :: [SyntaxGrammar.Alt NonTerminal Terminal elem action a]
    }

type Alt :: ([Type] -> Type -> Type) -> [(n, Type)] -> [t] -> Type -> Type -> Type
newtype Alt action rules tokens elem a = Alt
    {
        unsafeAlt :: SyntaxGrammar.Alt NonTerminal Terminal elem action a
    }

type Expr :: [(n, Type)] -> [t] -> Type -> [Type] -> Type
newtype Expr rules tokens elem us = Expr
    {
        unsafeExpr :: SyntaxGrammar.Expr NonTerminal Terminal elem us
    }

type Unit :: [(n, Type)] -> [t] -> Type -> Type -> Type
newtype Unit rules tokens elem u = Unit
    {
        unsafeUnit :: SyntaxGrammar.Unit NonTerminal Terminal elem u
    }

ruleExpr :: [Alt action rules tokens elem a] -> RuleExpr action rules tokens elem a
ruleExpr alts = RuleExpr do coerce alts

alt ::(Expr rules tokens elem us, action us a) -> Alt action rules tokens elem a
alt (Expr us, act) = Alt do SyntaxGrammar.Alt us act

(<^>) :: Unit rules tokens elem u -> (Expr rules tokens elem us1, action us2 a)
    -> (Expr rules tokens elem (u ': us1), action us2 a)
Unit u <^> (Expr us, act) = (Expr do u SyntaxGrammar.:^ us, act)

infixr 5 <^>

(<:>) :: Unit h q e u -> f us2 r -> (Expr h q e '[u], f us2 r)
Unit u <:> act = (Expr do u SyntaxGrammar.:^ SyntaxGrammar.Eps, act)

infixr 5 <:>

eps :: f '[] r -> Alt f h q e r
eps act = Alt do SyntaxGrammar.Alt SyntaxGrammar.Eps act

var :: forall v h q e. Record.RecordMember v h
    => Proxy v -> Unit h q e (TypeOps.FromJust (Record.RecordIndex v h))
var Proxy = Unit do SyntaxGrammar.UnitVar p where
    p = Record.unsafePosition
        do proxy# :: Proxy# v
        do proxy# :: Proxy# h

varA :: forall v h q e. Record.RecordMember v h
    => Unit h q e (TypeOps.FromJust (Record.RecordIndex v h))
varA = var do Proxy @v

tok :: forall t h q e. Member.T t q => Proxy t -> Unit h q e e
tok p = Unit
    do SyntaxGrammar.UnitToken
        do HEnum.unsafeHEnum do HEnum.henum p :: HEnum.T q

tokA :: forall t h q e. Member.T t q => Unit h q e e
tokA = tok do Proxy @t
