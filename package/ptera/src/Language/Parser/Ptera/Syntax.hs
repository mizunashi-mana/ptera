{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Parser.Ptera.Syntax where

import           Language.Parser.Ptera.Prelude

import qualified Data.Array                           as Array
import qualified Language.Parser.Ptera.Data.HList     as HList
import qualified Language.Parser.Ptera.Data.Member    as Member
import qualified Language.Parser.Ptera.Data.Record    as Record
import qualified Language.Parser.Ptera.Data.TypeOps   as TypeOps
import qualified Language.Parser.Ptera.Syntax.Grammar as SyntaxGrammar
import qualified Unsafe.Coerce                        as Unsafe

type T = Grammar

fixGrammar :: forall s h e. MemberInitials h s => Rules h e -> Grammar s h e
fixGrammar (Record.UnsafeRecord arr) = UnsafeGrammar do
    runIdentity do
        SyntaxGrammar.fixGrammarT do
            unsafeInitials
                do proxy# @h
                do proxy# @s

            forM_ do Array.assocs arr
                \(p, e) ->
                    let RuleExpr alts = Unsafe.unsafeCoerce e
                    in SyntaxGrammar.ruleT p do SyntaxGrammar.RuleExpr alts

newtype Grammar (s :: [n]) (h :: [(n, Type)]) e = UnsafeGrammar
    {
        unsafeGrammar ::
            SyntaxGrammar.FixedGrammar StartPoint NonTerminal (Terminal e) e SemanticAction
    }

class Enum (Terminal e) => GrammarToken e where
    data Terminal e :: Type

    tokenToTerminal :: e -> Terminal e

type StartPoint = Int
type NonTerminal = Int

type MemberInitials h s = MemberInitialsGo h s s

unsafeInitials :: MemberInitials h s
    => Proxy# h -> Proxy# s
    -> SyntaxGrammar.GrammarT StartPoint NonTerminal (Terminal e) e SemanticAction Identity ()
unsafeInitials hp# sp# = unsafeInitialsGo hp# sp# sp#

class MemberInitialsGo (h :: [(n, Type)]) (s :: [n]) (sg :: [n]) where
    unsafeInitialsGo :: Proxy# h -> Proxy# s -> Proxy# sg
        -> SyntaxGrammar.GrammarT StartPoint NonTerminal (Terminal e) e SemanticAction Identity ()

instance MemberInitialsGo h s '[] where
    unsafeInitialsGo _ _ _ = pure ()

instance (Member.T v s, Record.RecordMember v h, MemberInitialsGo h s sg)
        => MemberInitialsGo h s (v ': sg) where
    unsafeInitialsGo hp# sp# _ = do
        SyntaxGrammar.initialT sn vn
        unsafeInitialsGo hp# sp# do proxy# @sg
        where
            sn = Member.position
                do proxy# @v
                sp#
            vn = Record.unsafePosition
                do proxy# @v
                hp#


type Rules h e = Record.T (RulesTag h e)

type family RulesTag (h :: [(n, Type)]) e :: [(n, Type)] where
    RulesTag h e = TypeOps.MapMapSnd (RuleExpr h e) h

newtype RuleExpr (h :: [(n, Type)]) e r = RuleExpr
    {
        unsafeRuleExpr :: [SyntaxGrammar.Alt NonTerminal (Terminal e) e SemanticAction r]
    }

newtype Alt (h :: [(n, Type)]) e r = Alt
    {
        unsafeAlt :: SyntaxGrammar.Alt NonTerminal (Terminal e) e SemanticAction r
    }

newtype Expr (h :: [(n, Type)]) e us = Expr
    {
        unsafeExpr :: SyntaxGrammar.Expr NonTerminal (Terminal e) e us
    }

newtype Unit (h :: [(n, Type)]) e u = Unit
    {
        unsafeUnit :: SyntaxGrammar.Unit NonTerminal (Terminal e) e u
    }

newtype SemanticAction us a = SemanticAction
    {
        semanticAction :: HList.T us -> a
    }

ruleExpr :: [Alt h e r] -> RuleExpr h e r
ruleExpr alts = RuleExpr do coerce alts

alt ::(Expr h e us, SemanticAction us r) -> Alt h e r
alt (Expr us, act) = Alt do SyntaxGrammar.Alt us act

(<^>) :: Unit h e u -> (Expr h e us1, SemanticAction us2 r)
    -> (Expr h e (u ': us1), SemanticAction us2 r)
Unit u <^> (Expr us, act) = (Expr do u SyntaxGrammar.:^ us, act)

infixr 5 <^>

(<:>) :: Unit h e u -> (HList.T us2 -> r) -> (Expr h e '[u], SemanticAction us2 r)
Unit u <:> act = (Expr do u SyntaxGrammar.:^ SyntaxGrammar.Eps, SemanticAction act)

infixr 5 <:>

eps :: SemanticAction '[] r -> Alt n e r
eps act = Alt do SyntaxGrammar.Alt SyntaxGrammar.Eps act

var :: forall v h e r. Record.RecordMember v h => Proxy v -> Unit h e r
var Proxy = Unit do SyntaxGrammar.UnitVar p where
    p = Record.unsafePosition
        do proxy# @v
        do proxy# @h

varA :: forall v h e r. Record.RecordMember v h => Unit h e r
varA = var do Proxy @v

tok :: Terminal e -> Unit h e e
tok t = Unit do SyntaxGrammar.UnitToken t
