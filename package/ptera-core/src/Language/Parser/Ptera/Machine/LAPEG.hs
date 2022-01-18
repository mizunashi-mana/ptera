module Language.Parser.Ptera.Machine.LAPEG where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Symbolic.IntSet as SymbolicIntSet
import qualified Language.Parser.Ptera.Machine.PEG          as PEG


type T = LAPEG

data LAPEG start varDoc altDoc a = LAPEG
    { initials    :: EnumMap.EnumMap start VarNum
    , alts        :: AlignableArray.T AltNum (Alt altDoc a)
    , rules       :: AlignableArray.T VarNum Rule
    , vars        :: AlignableArray.T VarNum (PEG.Var varDoc)
    }
    deriving (Eq, Show, Functor)

data Rule = Rule
    {
        ruleRange :: LookAHeadRange,
        ruleAlts  :: [AltNum]
    }
    deriving (Eq, Show)

data LookAHeadRange = LookAHeadRange
    { lookAHeadEpsilon :: Bool
    , lookAHeadConsume :: SymbolicIntSet.T
    }
    deriving (Eq, Show)

instance Semigroup LookAHeadRange where
    lr1 <> lr2 = LookAHeadRange
        { lookAHeadEpsilon = lookAHeadEpsilon lr1 || lookAHeadEpsilon lr2
        , lookAHeadConsume = lookAHeadConsume lr1 <> lookAHeadConsume lr2
        }

instance Monoid LookAHeadRange where
    mempty = LookAHeadRange
        { lookAHeadEpsilon = False
        , lookAHeadConsume = mempty
        }

newtype AltNum = AltNum Int
    deriving (Eq, Show)
    deriving Hashable via Int
    deriving Alignable.T via Alignable.Inst

data Alt altDoc a = Alt
    { altVar     :: VarNum
    , altKind    :: PEG.AltKind
    , altUnitSeq :: AlignableArray.T Position Unit
    , altAction  :: a
    , altHelp    :: altDoc
    }
    deriving (Eq, Show, Functor)

newtype Position = Position Int
    deriving (Eq, Show)
    deriving Hashable via Int
    deriving Alignable.T via Alignable.Inst

newtype VarNum = VarNum Int
    deriving (Eq, Show)
    deriving Hashable via Int
    deriving Alignable.T via Alignable.Inst

data Unit
    = UnitTerminal PEG.Terminal
    | UnitNonTerminal VarNum
    | UnitNot
    deriving (Eq, Show)
