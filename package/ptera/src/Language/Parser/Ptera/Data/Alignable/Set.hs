module Language.Parser.Ptera.Data.Alignable.Set (
    T,
    Set,
    empty,
    singleton,
    insert,
    fromList,
    toList,
    null,
    intersection,
    union,
    length,
    member,
) where

import           Language.Parser.Ptera.Prelude hiding (empty, toList, null, length)

import qualified Language.Parser.Ptera.Data.Alignable as Alignable
import qualified Data.IntSet                 as IntSet


type T = Set

newtype Set n = Set IntSet.IntSet
    deriving (Eq, Show)

empty :: Set n
empty = coerce IntSet.empty

singleton :: Alignable.T n => n -> Set n
singleton = coerce IntSet.singleton

insert :: Alignable.T n => n -> Set n -> Set n
insert = coerce IntSet.insert

fromList :: Alignable.T n => [n] -> Set n
fromList = coerce IntSet.fromList

toList :: Alignable.T n => Set n -> [n]
toList = coerce IntSet.toList

null :: Set n -> Bool
null = coerce IntSet.null

intersection :: Set n -> Set n -> Set n
intersection = coerce IntSet.intersection

difference :: Set n -> Set n -> Set n
difference = coerce IntSet.difference

union :: Set n -> Set n -> Set n
union = coerce IntSet.union

length :: Set n -> Int
length = coerce IntSet.size

member :: Alignable.T n => n -> Set n -> Bool
member = coerce IntSet.member
