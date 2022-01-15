module Language.Parser.Ptera.Machine.PEG where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray


type T = PEG

data PEG start varDoc altDoc a = PEG
    { vars        :: AlignableArray.T VarNum (Var varDoc)
    , rules       :: AlignableArray.T VarNum (Rule altDoc a)
    , initials    :: EnumMap.EnumMap start VarNum
    }
    deriving (Eq, Show, Functor)

newtype Rule altDoc a = Rule [Alt altDoc a]
    deriving (Eq, Show, Functor)

newtype Var varDoc = Var
    { varHelp :: varDoc
    }
    deriving (Eq, Show)

data Alt altDoc a = Alt
    {
        altKind    :: AltKind,
        altUnitSeq :: [Unit],
        altAction  :: a,
        altHelp    :: altDoc
    }
    deriving (Eq, Show, Functor)

data AltKind
    = AltSeq
    | AltNot
    | AltAnd
    deriving (Eq, Show)

data Unit
    = UnitTerminal Terminal
    | UnitNonTerminal VarNum
    deriving (Eq, Show)

type Terminal = Int

newtype VarNum = VarNum Int
    deriving (Eq, Show)
    deriving Enum via Int
    deriving Alignable.T via Alignable.Inst
