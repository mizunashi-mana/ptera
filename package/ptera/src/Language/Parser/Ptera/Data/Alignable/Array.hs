module Language.Parser.Ptera.Data.Alignable.Array where

import           Language.Parser.Ptera.Prelude

import qualified Data.Array                               as DataArray
import qualified Language.Parser.Ptera.Data.Alignable     as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Map as AlignableMap


type T = Array

newtype Array n a = Array (DataArray.Array Int a)
    deriving (Eq, Show, Functor, Foldable)

fromTotalMap :: Alignable.T n => n -> AlignableMap.T n a -> Array n a
fromTotalMap b m = Array
    do DataArray.array (0, pred do coerce b) do coerce do AlignableMap.toAscList m

fromList :: forall n a. Alignable.T n => [a] -> Array n a
fromList xs = Array
    do DataArray.listArray (0, length xs - 1) xs

mapWithIx :: Alignable.T n => (n -> a -> a) -> Array n a -> Array n a
mapWithIx f (Array arr) = Array
    do DataArray.listArray
        do DataArray.bounds arr
        do [ f (coerce i) x | (i, x) <- DataArray.assocs arr ]

index :: forall n a. Alignable.T n => Array n a -> n -> a
index = coerce do (DataArray.!) @Int @a

assocs :: forall n a. Alignable.T n => Array n a -> [(n, a)]
assocs = coerce do DataArray.assocs @Int @a
