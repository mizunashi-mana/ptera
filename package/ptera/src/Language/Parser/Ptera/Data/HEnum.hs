{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.Parser.Ptera.Data.HEnum where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Data.Member as Member

type T = HEnum

newtype HEnum (as :: [k]) = UnsafeHEnum
    {
        unsafeHEnum :: Int
    }
    deriving (Eq, Show)

henum :: forall a as. Member.T a as => Proxy a -> HEnum as
henum Proxy = UnsafeHEnum
    do Member.position
        do proxy# :: Proxy# a
        do proxy# :: Proxy# as

henumA :: forall a as. Member.T a as => HEnum as
henumA = henum do Proxy @a

unHEnum :: forall a as. Member.T a as => Proxy a -> HEnum as -> Bool
unHEnum Proxy (UnsafeHEnum i) = i == Member.position
    do proxy# :: Proxy# a
    do proxy# :: Proxy# as
