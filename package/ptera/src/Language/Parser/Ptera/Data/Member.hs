{-# LANGUAGE UndecidableInstances #-}

module Language.Parser.Ptera.Data.Member where

import Language.Parser.Ptera.Prelude

import qualified GHC.TypeNats as TypeNats
import qualified Language.Parser.Ptera.Data.TypeOps as TypeOps

type T = Member

class Member (a :: k) (as :: [k]) where
    type Position a as :: TypeNats.Nat

    position :: Proxy# a -> Proxy# as -> Int

instance TypeNats.KnownNat (Position a as) => Member a as where
    type Position a as = TypeOps.FromJust (TypeOps.Find a as)

    position _ _ = fromInteger
        do toInteger
            do TypeNats.natVal'
                do proxy# :: Proxy# (Position a as)
