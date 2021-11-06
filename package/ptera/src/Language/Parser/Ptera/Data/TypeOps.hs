{-# LANGUAGE UndecidableInstances #-}

module Language.Parser.Ptera.Data.TypeOps where

import           Language.Parser.Ptera.Prelude

import qualified GHC.TypeNats                  as TypeNats

type family FromJust (m :: Maybe k) :: k where
    FromJust ('Just a) = a

type family Index (n :: TypeNats.Nat) (as :: [k]) :: Maybe k where
    Index _ '[]           = 'Nothing
    Index 0 (a ': as)     = 'Just a
    Index n (_ ': as)     = Index (n TypeNats.- 1) as

type family Find (a :: k) (as :: [k]) :: Maybe TypeNats.Nat where
    Find _ '[]           = 'Nothing
    Find a (a ': as)     = 'Just (Length as)
    Find a (_ ': as)     = Find a as

type family Length (as :: [k]) :: TypeNats.Nat where
    Length '[] = 0
    Length (_ ': as) = 1 TypeNats.+ Length as

type family MapFst (as :: [(k1, k2)]) :: [k1] where
    MapFst '[] = '[]
    MapFst ('(n, a) ': as) = a ': MapFst as

type family MapSnd (as :: [(k1, k2)]) :: [k2] where
    MapSnd '[] = '[]
    MapSnd ('(n, a) ': as) = a ': MapSnd as
