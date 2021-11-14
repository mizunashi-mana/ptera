module Language.Parser.Ptera.Data.Alignable.Map (
    T,
    Map,
    empty,
    singleton,
    insert,
    lookup,
    assocs,
    toAscList,
    restrictGreaterOrEqual,
) where

import           Language.Parser.Ptera.Prelude        hiding (empty, lookup)

import qualified Data.IntMap.Strict                   as IntMap
import qualified Language.Parser.Ptera.Data.Alignable as Alignable
import qualified Language.Parser.Ptera.Data.IntMap.GreaterRestriction as GreaterRestriction


type T = Map

newtype Map n a = Map (IntMap.IntMap a)
    deriving (Eq, Show, Functor, Foldable, Traversable)

empty :: Map n a
empty = Map IntMap.empty

singleton :: forall n a. Alignable.T n => n -> a -> Map n a
singleton = coerce do IntMap.singleton @a

insert :: forall n a. Alignable.T n => n -> a -> Map n a -> Map n a
insert = coerce do IntMap.insert @a

lookup :: forall n a. Alignable.T n => n -> Map n a -> Maybe a
lookup = coerce do IntMap.lookup @a

assocs :: forall n a. Alignable.T n => Map n a -> [(n, a)]
assocs = coerce do IntMap.assocs @a

toAscList :: forall n a. Alignable.T n => Map n a -> [(n, a)]
toAscList = coerce do IntMap.toAscList @a

restrictGreaterOrEqual :: forall n a. Alignable.T n => n -> Map n a -> Map n a
restrictGreaterOrEqual n (Map m) = Map do
    GreaterRestriction.restrictGreater (coerce n - 1) m
