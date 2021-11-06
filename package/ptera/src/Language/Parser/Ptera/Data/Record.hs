module Language.Parser.Ptera.Data.Record where

import Language.Parser.Ptera.Prelude

import qualified Data.Array as Array
import qualified Language.Parser.Ptera.Data.HList as HList
import qualified Language.Parser.Ptera.Data.Member as Member
import qualified Language.Parser.Ptera.Data.TypeOps as TypeOps
import qualified Unsafe.Coerce as Unsafe

type T = Record

data Record :: [(k, Type)] -> Type where
    UnsafeRecord :: Array.Array Int e -> Record as

fromHList :: Proxy# as -> HList.T (TypeOps.MapSnd as) -> Record as
fromHList _ = \xs -> go0 do go1 xs where
    go0 xs = UnsafeRecord do
        Array.listArray (0, length xs - 1) xs

    go1 :: HList.T as -> [e]
    go1 = \case
        HList.HNil ->
            []
        x HList.:* xs ->
            Unsafe.unsafeCoerce x:go1 xs

index :: forall k as. Member.T k (TypeOps.MapFst as)
    => Proxy# k -> Record as -> TypeOps.FromJust (RecordIndex k as)
index kp (UnsafeRecord xs) = Unsafe.unsafeCoerce do
    xs Array.! Member.position kp (proxy# :: Proxy# (TypeOps.MapFst as))

type family RecordIndex (n :: k) (h :: [(k, v)]) :: Maybe v where
    RecordIndex _ '[]               = 'Nothing
    RecordIndex n ('(n, a) ': _)    = 'Just a
    RecordIndex n (_ ': h)          = RecordIndex n h
