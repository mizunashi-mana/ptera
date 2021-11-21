module Language.Parser.Ptera.Data.Record where

import           Language.Parser.Ptera.Prelude

import qualified Data.Array                         as Array
import qualified Language.Parser.Ptera.Data.HList   as HList
import qualified Language.Parser.Ptera.Data.Member  as Member
import qualified Language.Parser.Ptera.Data.TypeOps as TypeOps
import qualified Unsafe.Coerce                      as Unsafe

type T = Record

data Record :: [(k, Type)] -> Type where
    UnsafeRecord :: Array.Array Int e -> Record h

newtype Field k v = Field v

data FieldLabel k = FieldLabel

field :: FieldLabel k -> v -> Field k v
field FieldLabel x = Field x

instance IsLabel k (FieldLabel k) where
    fromLabel = FieldLabel

type RecordMember k h = Member.T k (TypeOps.MapFst h)

unsafePosition :: forall k h. RecordMember k h => Proxy# k -> Proxy# h -> Int
unsafePosition kp# _ = Member.position kp# do proxy# @(TypeOps.MapFst h)

fromFields :: forall h. FromFields h => Proxy h -> HList.T (Fields h) -> Record h
fromFields Proxy = \xs -> go0 do
        fromFieldsGo
            do proxy# @h
            [] xs
    where
        go0 xs = UnsafeRecord do
            Array.listArray (0, length xs - 1) xs

class FromFields (h :: [(k, Type)]) where
    fromFieldsGo :: Proxy# h -> [e] -> HList.T (Fields h) -> [e]

instance FromFields '[] where
    fromFieldsGo _ acc HList.HNil = acc

instance FromFields h => FromFields ('(n, a) ': h) where
    fromFieldsGo _ acc = \case
        Field x HList.:* rest -> fromFieldsGo
            do proxy# @h
            do Unsafe.unsafeCoerce x:acc
            rest

type family Fields (h :: [(k, Type)]) :: [Type] where
    Fields '[] = '[]
    Fields ('(n, a) ': h) = Field n a ': Fields h

fromHList :: Proxy h -> HList.T (TypeOps.MapSnd h) -> Record h
fromHList Proxy = \xs -> go0 do go1 [] xs where
    go0 xs = UnsafeRecord do
        Array.listArray (0, length xs - 1) xs

    go1 :: [e] -> HList.T as -> [e]
    go1 acc = \case
        HList.HNil ->
            acc
        x HList.:* xs ->
            go1
                do Unsafe.unsafeCoerce x:acc
                xs

index :: forall k h. RecordMember k h
    => Proxy k -> Record h -> TypeOps.FromJust (RecordIndex k h)
index Proxy (UnsafeRecord xs) = Unsafe.unsafeCoerce do
    xs Array.! unsafePosition
        do proxy# @k
        do proxy# @h

type family RecordIndex (n :: k) (h :: [(k, v)]) :: Maybe v where
    RecordIndex _ '[]               = 'Nothing
    RecordIndex n ('(n, a) ': _)    = 'Just a
    RecordIndex n (_ ': h)          = RecordIndex n h
