module Language.Parser.Ptera.Machine.LAPEG where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Symbolic.IntSet as SymbolicIntSet
import qualified Language.Parser.Ptera.Machine.PEG          as PEG


type T = LAPEG

data LAPEG s a = LAPEG
    {
        initials :: EnumMap.EnumMap s Var,
        alts     :: AlignableArray.T AltNum (Alt a),
        rules    :: AlignableArray.T Var Rule
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

data Alt a = Alt
    {
        altVar     :: Var,
        altKind    :: PEG.AltKind,
        altUnitSeq :: AlignableArray.T Position Unit,
        altAction  :: a
    }
    deriving (Eq, Show, Functor)

newtype Position = Position Int
    deriving (Eq, Show)
    deriving Hashable via Int
    deriving Alignable.T via Alignable.Inst

newtype Var = Var Int
    deriving (Eq, Show)
    deriving Alignable.T via Alignable.Inst

data Unit
    = UnitTerminal PEG.Terminal
    | UnitNonTerminal Var
    | UnitNot
    deriving (Eq, Show)
