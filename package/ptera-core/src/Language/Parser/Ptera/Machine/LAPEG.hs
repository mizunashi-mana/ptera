module Language.Parser.Ptera.Machine.LAPEG where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Symbolic.IntSet as SymbolicIntSet
import qualified Language.Parser.Ptera.Machine.PEG          as PEG


type T = LAPEG

data LAPEG start varDoc altDoc a = LAPEG
    { vars     :: AlignableArray.T VarNum (PEG.Var varDoc)
    , rules    :: AlignableArray.T VarNum Rule
    , alts     :: AlignableArray.T AltNum (Alt altDoc a)
    , initials :: EnumMap.EnumMap start VarNum
    }
    deriving (Eq, Show, Functor)

newtype VarNum = VarNum Int
    deriving (Eq, Show)
    deriving Hashable via Int
    deriving Alignable.T via Alignable.Inst

newtype AltNum = AltNum Int
    deriving (Eq, Show)
    deriving Hashable via Int
    deriving Alignable.T via Alignable.Inst

data Rule = Rule
    { ruleRange :: HeadRange
    , ruleAlts  :: [AltNum]
    }
    deriving (Eq, Show)

data Alt altDoc a = Alt
    { altVar                  :: VarNum
    , altKind                 :: PEG.AltKind
    , altUnitSeqWithLookAHead :: AlignableArray.T Position (HeadRange, Unit)
    , altAction               :: a
    , altHelp                 :: altDoc
    }
    deriving (Eq, Show, Functor)

newtype Position = Position Int
    deriving (Eq, Show)
    deriving Hashable via Int
    deriving Alignable.T via Alignable.Inst

data HeadRange = HeadRange
    { headRangeEpsilon :: Bool
    , headRangeConsume :: SymbolicIntSet.T
    }
    deriving (Eq, Show)

instance Semigroup HeadRange where
    hr1 <> hr2 = HeadRange
        { headRangeEpsilon = headRangeEpsilon hr1 || headRangeEpsilon hr2
        , headRangeConsume = headRangeConsume hr1 <> headRangeConsume hr2
        }

instance Monoid HeadRange where
    mempty = HeadRange
        { headRangeEpsilon = False
        , headRangeConsume = mempty
        }

data Unit
    = UnitTerminal Terminal
    | UnitNonTerminal VarNum
    | UnitNot
    deriving (Eq, Show)

type Terminal = Int
