module Language.Parser.Ptera.Data.Alignable.Map (
    T,
    Map,
    empty,
    insert,
    lookup,
    assocs,
    toAscList,
) where

import           Language.Parser.Ptera.Prelude        hiding (empty, lookup)

import qualified Data.IntMap.Strict                   as IntMap
import qualified Language.Parser.Ptera.Data.Alignable as Alignable


type T = Map

newtype Map n a = Map (IntMap.IntMap a)
    deriving (Eq, Show, Functor, Foldable, Traversable)

empty :: Map n a
empty = Map IntMap.empty

insert :: forall n a. Alignable.T n => n -> a -> Map n a -> Map n a
insert = coerce do IntMap.insert @a

lookup :: forall n a. Alignable.T n => n -> Map n a -> Maybe a
lookup = coerce do IntMap.lookup @a

assocs :: forall n a. Alignable.T n => Map n a -> [(n, a)]
assocs = coerce do IntMap.assocs @a

toAscList :: forall n a. Alignable.T n => Map n a -> [(n, a)]
toAscList = coerce do IntMap.toAscList @a


