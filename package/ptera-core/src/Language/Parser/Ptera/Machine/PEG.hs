module Language.Parser.Ptera.Machine.PEG where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray


type T = PEG

data PEG start varDoc altDoc a = PEG
    { vars        :: AlignableArray.T VarNum (Var varDoc)
    , rules       :: AlignableArray.T VarNum Rule
    , alts        :: AlignableArray.T AltNum (Alt altDoc a)
    , initials    :: EnumMap.EnumMap start VarNum
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

newtype Rule = Rule
    { ruleAlts :: [AltNum]
    }
    deriving (Eq, Show)

newtype Var varDoc = Var
    { varHelp :: varDoc
    }
    deriving (Eq, Show, Functor)

data Alt altDoc a = Alt
    { altVar     :: VarNum
    , altKind    :: AltKind
    , altUnitSeq :: AlignableArray.T Position Unit
    , altAction  :: a
    , altHelp    :: altDoc
    }
    deriving (Eq, Show, Functor)

data AltKind
    = AltSeq
    | AltNot
    | AltAnd
    deriving (Eq, Show)

newtype Position = Position Int
    deriving (Eq, Show)
    deriving Hashable via Int
    deriving Alignable.T via Alignable.Inst

data Unit
    = UnitTerminal Terminal
    | UnitNonTerminal VarNum
    | UnitNot
    deriving (Eq, Show)

type Terminal = Int
