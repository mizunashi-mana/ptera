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

fixGrammar :: forall f s h q e. MemberInitials h s => Rules f h q e -> Grammar f s h q e
fixGrammar (Record.UnsafeRecord arr) = UnsafeGrammar do
    runIdentity do
        SyntaxGrammar.fixGrammarT do
            unsafeInitials
                do proxy# :: Proxy# h
                do proxy# :: Proxy# s

            forM_ do Array.assocs arr
                \(p, e) ->
                    let RuleExpr alts = Unsafe.unsafeCoerce e
                    in SyntaxGrammar.ruleT p do SyntaxGrammar.RuleExpr alts

newtype Grammar (f :: [Type] -> Type -> Type) (s :: [n]) (h :: [(n, Type)]) (q :: [t]) e = UnsafeGrammar
    {
        unsafeGrammar ::
            SyntaxGrammar.FixedGrammar StartPoint NonTerminal Terminal e f
    }

type StartPoint = Int
type Terminal = Int
type NonTerminal = Int

type MemberInitials h s = MemberInitialsGo h s s

unsafeInitials :: MemberInitials h s
    => Proxy# h -> Proxy# s
    -> SyntaxGrammar.GrammarT StartPoint NonTerminal Terminal e f Identity ()
unsafeInitials hp# sp# = unsafeInitialsGo hp# sp# sp#

class MemberInitialsGo (h :: [(n, Type)]) (s :: [n]) (sg :: [n]) where
    unsafeInitialsGo :: Proxy# h -> Proxy# s -> Proxy# sg
        -> SyntaxGrammar.GrammarT StartPoint NonTerminal Terminal e f Identity ()

instance MemberInitialsGo h s '[] where
    unsafeInitialsGo _ _ _ = pure ()

instance (Member.T v s, Record.RecordMember v h, MemberInitialsGo h s sg)
        => MemberInitialsGo h s (v ': sg) where
    unsafeInitialsGo hp# sp# _ = do
        SyntaxGrammar.initialT sn vn
        unsafeInitialsGo hp# sp# do proxy# :: Proxy# sg
        where
            sn = Member.position
                do proxy# :: Proxy# v
                sp#
            vn = Record.unsafePosition
                do proxy# :: Proxy# v
                hp#


type Rules f h q e = Record.T (RulesTag f h q e)

type family RulesTag (f :: [Type] -> Type -> Type) (h :: [(n, Type)]) (q :: [t]) (e :: Type) :: [(n, Type)] where
    RulesTag f h q e = TypeOps.MapMapSnd (RuleExpr f h q e) h

newtype RuleExpr f (h :: [(n, Type)]) (q :: [t]) e r = RuleExpr
    {
        unsafeRuleExpr :: [SyntaxGrammar.Alt NonTerminal Terminal e f r]
    }

newtype Alt f (h :: [(n, Type)]) (q :: [t]) e r = Alt
    {
        unsafeAlt :: SyntaxGrammar.Alt NonTerminal Terminal e f r
    }

newtype Expr (h :: [(n, Type)]) (q :: [t]) e us = Expr
    {
        unsafeExpr :: SyntaxGrammar.Expr NonTerminal Terminal e us
    }

newtype Unit (h :: [(n, Type)]) (q :: [t]) e u = Unit
    {
        unsafeUnit :: SyntaxGrammar.Unit NonTerminal Terminal e u
    }

ruleExpr :: [Alt f h q e r] -> RuleExpr f h q e r
ruleExpr alts = RuleExpr do coerce alts

alt ::(Expr h q e us, f us r) -> Alt f h q e r
alt (Expr us, act) = Alt do SyntaxGrammar.Alt us act

(<^>) :: Unit h q e u -> (Expr h q e us1, f us2 r)
    -> (Expr h q e (u ': us1), f us2 r)
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
