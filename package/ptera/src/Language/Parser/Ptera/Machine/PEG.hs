module Language.Parser.Ptera.Machine.PEG where

import Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray


type T = PEG

newtype StartPoint = StartPoint Int
    deriving (Eq, Show)
    deriving Enum via Int

data PEG a = PEG
    {
        pegInitials :: EnumMap.EnumMap StartPoint Var,
        pegRules :: AlignableArray.T Var (PE a)
    }
    deriving (Eq, Show)

newtype PE a = PE (NonEmpty (Alt a))
    deriving (Eq, Show)

data Alt a = Alt
    {
        altKind :: AltKind,
        altUnitSeq :: [Unit],
        altAction :: a
    }
    deriving (Eq, Show)

data AltKind
    = AltSeq
    | AltNot
    | AltAnd
    deriving (Eq, Show)

data Unit
    = UnitTerminal Terminal
    | UnitNonTerminal Var
    deriving (Eq, Show)

newtype Terminal = Terminal Int
    deriving (Eq, Show)
    deriving Enum via Int

newtype Var = Var Int
    deriving (Eq, Show)
    deriving Enum via Int
    deriving Alignable.T via Alignable.Inst
