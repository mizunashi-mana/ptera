{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.Parser.Ptera.Data.HEnum where

import           Language.Parser.Ptera.Prelude

import qualified Type.Membership as Membership
import qualified Type.Membership.Internal as MembershipInternal

type T = HEnum

newtype HEnum (as :: [k]) = UnsafeHEnum
    {
        unsafeHEnum :: Int
    }
    deriving (Eq, Show)

henum :: forall a as. Membership.Membership as a -> HEnum as
henum m = UnsafeHEnum do Membership.getMemberId m

henumA :: forall a as. Membership.Member as a => HEnum as
henumA = henum do MembershipInternal.membership @as @a

unHEnum :: forall a as. Membership.Membership as a -> HEnum as -> Bool
unHEnum m (UnsafeHEnum i) = Membership.getMemberId m == i
