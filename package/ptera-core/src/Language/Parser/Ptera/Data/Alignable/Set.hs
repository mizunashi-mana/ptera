module Language.Parser.Ptera.Data.Alignable.Set (
    T,
    Set,
    empty,
    singleton,
    insert,
    delete,
    fromList,
    toList,
    null,
    intersection,
    union,
    difference,
    length,
    member,
) where

import           Language.Parser.Ptera.Prelude        hiding (empty, length,
                                                       null, toList)

import qualified Data.IntSet                          as IntSet
import qualified Language.Parser.Ptera.Data.Alignable as Alignable


type T = Set

newtype Set n = Set IntSet.IntSet
    deriving (Eq, Show)
    deriving Semigroup via IntSet.IntSet

empty :: Set n
empty = coerce IntSet.empty

singleton :: Alignable.T n => n -> Set n
singleton = coerce IntSet.singleton

insert :: Alignable.T n => n -> Set n -> Set n
insert = coerce IntSet.insert

delete :: Alignable.T n => n -> Set n -> Set n
delete = coerce IntSet.delete

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
