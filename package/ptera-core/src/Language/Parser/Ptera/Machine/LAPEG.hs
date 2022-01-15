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
        ruleRange :: SymbolicIntSet.T,
        ruleAlts  :: [AltNum]
    }
    deriving (Eq, Show)

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
