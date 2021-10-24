module Language.Parser.Ptera.Machine.LAPEG where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Symbolic.IntMap as SymbolicIntMap
import qualified Language.Parser.Ptera.Data.Symbolic.IntSet as SymbolicIntSet
import qualified Language.Parser.Ptera.Machine.PEG          as PEG


type T = LAPEG

data LAPEG a = LAPEG
    {
        initials :: EnumMap.EnumMap PEG.StartPoint Var,
        rules    :: AlignableArray.T Var (LAPE a)
    }
    deriving (Eq, Show, Functor)

data LAPE a = LAPE
    {
        available :: SymbolicIntSet.T,
        alts      :: SymbolicIntMap.T (NonEmpty (Alt a))
    }
    deriving (Eq, Show, Functor)

data Alt a = Alt
    {
        altKind    :: PEG.AltKind,
        altUnitSeq :: [Unit],
        altAction  :: a
    }
    deriving (Eq, Show, Functor)

newtype Var = Var Int
    deriving (Eq, Show)
    deriving Enum via Int
    deriving Alignable.T via Alignable.Inst

data Unit
    = UnitTerminal PEG.Terminal
    | UnitNonTerminal Var
    deriving (Eq, Show)
