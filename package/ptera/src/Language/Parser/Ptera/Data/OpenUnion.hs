{-# LANGUAGE UndecidableInstances #-}

module Language.Parser.Ptera.Data.OpenUnion where

import Language.Parser.Ptera.Prelude

import qualified GHC.TypeNats as TypeNats
import qualified Unsafe.Coerce as Unsafe

type Fields k = [(k, Type)]

data OpenUnion :: Fields k -> Type where
    UnsafeOpenUnion :: Int -> a -> OpenUnion us

class Member (us :: Fields k) (n :: k) where
    memberInjectPosition :: Proxy# us -> Proxy# n -> Int
    inject :: Proxy# us -> Proxy# n -> FromJust (Index n us) -> OpenUnion us
    project :: Proxy# n -> OpenUnion us -> Maybe (FromJust (Index n us))

instance InjectPosition us n => Member us n where
    memberInjectPosition usp np = injectPosition usp np
    inject usp np x = UnsafeOpenUnion
        do memberInjectPosition usp np
        do x
    project np (UnsafeOpenUnion i x)
        | i == memberInjectPosition (proxy# :: Proxy# us) np =
            Just do Unsafe.unsafeCoerce x
        | otherwise =
            Nothing

class InjectPosition (us :: Fields k) (n :: k) where
    injectPosition :: Proxy# us -> Proxy# n -> Int

instance TypeNats.KnownNat (FromJust (IndexOf n us)) => InjectPosition us n where
    injectPosition _ _ = fromInteger
        do toInteger do TypeNats.natVal' @(FromJust (IndexOf n us)) proxy#

type family FromJust (m :: Maybe k) :: k where
    FromJust ('Just x) = x

type family IndexOf (n :: k) (us :: Fields k) :: Maybe TypeNats.Nat where
    IndexOf _ '[]               = 'Nothing
    IndexOf n ('(n, x) ': us)   = 'Just (Length us)
    IndexOf n (_ ': us)         = IndexOf n us

type family Index (n :: k) (us :: Fields k) :: Maybe Type where
    Index _ '[]             = 'Nothing
    Index n ('(n, x):us)    = 'Just x
    Index n (_:us)          = Index n us

type family Length (us :: Fields k) :: TypeNats.Nat where
    Length '[] = 0
    Length (_ ': xs) = 1 TypeNats.+ Length xs
