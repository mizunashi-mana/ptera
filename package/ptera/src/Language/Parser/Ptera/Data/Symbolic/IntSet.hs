module Language.Parser.Ptera.Data.Symbolic.IntSet where

import           Language.Parser.Ptera.Prelude hiding (empty)

import qualified Data.IntSet                   as DataIntSet


type T = IntSet

type Key = Int

data IntSet
    = StraightSet DataIntSet.IntSet
    | NegativeSet DataIntSet.IntSet
    deriving (Eq, Show)

instance Semigroup IntSet where
    (<>) = union

instance Monoid IntSet where
    mempty = StraightSet DataIntSet.empty

full :: IntSet
full = NegativeSet DataIntSet.empty

singleton :: Key -> IntSet
singleton k = StraightSet do DataIntSet.singleton k

invert :: IntSet -> IntSet
invert = \case
    StraightSet s -> NegativeSet s
    NegativeSet s -> StraightSet s

fromList :: [Key] -> IntSet
fromList ks = StraightSet do DataIntSet.fromList ks

insert :: Key -> IntSet -> IntSet
insert k = \case
    StraightSet s -> StraightSet do DataIntSet.insert k s
    NegativeSet s -> NegativeSet do DataIntSet.delete k s

delete :: Key -> IntSet -> IntSet
delete k = \case
    StraightSet s -> StraightSet do DataIntSet.delete k s
    NegativeSet s -> NegativeSet do DataIntSet.insert k s

member :: Key -> IntSet -> Bool
member k = \case
    StraightSet s -> DataIntSet.member k s
    NegativeSet s -> not do DataIntSet.member k s

union :: IntSet -> IntSet -> IntSet
union (StraightSet s1) (StraightSet s2) = StraightSet do DataIntSet.union s1 s2
union (StraightSet s1) (NegativeSet s2) = NegativeSet do DataIntSet.difference s2 s1
union (NegativeSet s1) (StraightSet s2) = NegativeSet do DataIntSet.difference s1 s2
union (NegativeSet s1) (NegativeSet s2) = NegativeSet do DataIntSet.intersection s1 s2

intersection :: IntSet -> IntSet -> IntSet
intersection (StraightSet s1) (StraightSet s2) = StraightSet do DataIntSet.intersection s1 s2
intersection (StraightSet s1) (NegativeSet s2) = StraightSet do DataIntSet.difference s1 s2
intersection (NegativeSet s1) (StraightSet s2) = StraightSet do DataIntSet.difference s2 s1
intersection (NegativeSet s1) (NegativeSet s2) = NegativeSet do DataIntSet.union s1 s2

difference :: IntSet -> IntSet -> IntSet
difference (StraightSet s1) (StraightSet s2) = StraightSet do DataIntSet.difference s1 s2
difference (StraightSet s1) (NegativeSet s2) = StraightSet do DataIntSet.intersection s1 s2
difference (NegativeSet s1) (StraightSet s2) = NegativeSet do DataIntSet.union s1 s2
difference (NegativeSet s1) (NegativeSet s2) = StraightSet do DataIntSet.difference s2 s1
