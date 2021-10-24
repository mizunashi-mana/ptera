module Language.Parser.Ptera.Machine.LAPEG where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Symbolic.IntSet as SymbolicIntSet
import qualified Language.Parser.Ptera.Machine.PEG          as PEG
import qualified Data.HashMap.Strict as HashMap


type T = LAPEG

data LAPEG a = LAPEG
    {
        initials :: EnumMap.EnumMap PEG.StartPoint Var,
        alts :: AlignableArray.T AltNum (Alt a),
        rules    :: AlignableArray.T Var Rule
    }
    deriving (Eq, Show, Functor)

newtype Rule = Rule (HashMap.HashMap (NonEmpty AltNum) SymbolicIntSet.T)
    deriving (Eq, Show)

newtype AltNum = AltNum Int
    deriving (Eq, Show)
    deriving Hashable via Int
    deriving Alignable.T via Alignable.Inst

data Alt a = Alt
    {
        altKind    :: PEG.AltKind,
        altUnitSeq :: [Unit],
        altAction  :: a
    }
    deriving (Eq, Show, Functor)

newtype Var = Var Int
    deriving (Eq, Show)
    deriving Alignable.T via Alignable.Inst

data Unit
    = UnitTerminal PEG.Terminal
    | UnitNonTerminal Var
    deriving (Eq, Show)
