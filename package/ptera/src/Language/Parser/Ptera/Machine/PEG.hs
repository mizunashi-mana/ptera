module Language.Parser.Ptera.Machine.PEG where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray


type T = PEG

data PEG a = PEG
    {
        initials :: EnumMap.EnumMap StartPoint Var,
        rules    :: AlignableArray.T Var (Rule a)
    }
    deriving (Eq, Show, Functor)

newtype StartPoint = StartPoint Int
    deriving (Eq, Show)
    deriving Enum via Int

newtype Rule a = Rule [Alt a]
    deriving (Eq, Show, Functor)

data Alt a = Alt
    {
        altKind    :: AltKind,
        altUnitSeq :: [Unit],
        altAction  :: a
    }
    deriving (Eq, Show, Functor)

data AltKind
    = AltSeq
    | AltNot
    | AltAnd
    deriving (Eq, Show)

data Unit
    = UnitTerminal Terminal
    | UnitNonTerminal Var
    deriving (Eq, Show)

type Terminal = Int

newtype Var = Var Int
    deriving (Eq, Show)
    deriving Enum via Int
    deriving Alignable.T via Alignable.Inst
