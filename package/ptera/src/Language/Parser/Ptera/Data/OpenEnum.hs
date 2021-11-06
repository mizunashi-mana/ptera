{-# LANGUAGE UndecidableInstances #-}

module Language.Parser.Ptera.Data.OpenEnum where

import Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Data.Member as Member

type T = OpenEnum

data OpenEnum :: [k] -> Type where
    UnsafeOpenEnum :: Int -> OpenEnum es

class OpenEnumOps (e :: k) (es :: [k]) where
    inject :: Proxy# e -> Proxy# es -> OpenEnum es
    project :: Proxy# e -> OpenEnum es -> Bool

instance Member.T e es => OpenEnumOps e es where
    inject ep esp =
        UnsafeOpenEnum do Member.position ep esp
    project ep (UnsafeOpenEnum i) =
        i == Member.position ep do proxy# :: Proxy# es
