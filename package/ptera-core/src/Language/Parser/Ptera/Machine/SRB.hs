module Language.Parser.Ptera.Machine.SRB where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Symbolic.IntMap as SymbolicIntMap
import qualified Language.Parser.Ptera.Machine.LAPEG        as LAPEG
import qualified Language.Parser.Ptera.Machine.PEG        as PEG


type T = SRB

data SRB start varDoc altDoc a = SRB
    { initials    :: EnumMap.EnumMap start StateNum
    , states      :: AlignableArray.T StateNum MState
    , alts        :: AlignableArray.T LAPEG.AltNum (LAPEG.Alt altDoc a)
    , vars :: AlignableArray.T LAPEG.VarNum (PEG.Var varDoc)
    }
    deriving (Eq, Show, Functor)

newtype StateNum = StateNum Int
    deriving (Eq, Show)
    deriving Hashable via Int
    deriving Alignable.T via Alignable.Inst

data MState = MState
    {
        stateNum      :: StateNum,
        stateTrans    :: SymbolicIntMap.T Trans,
        stateAltItems :: [AltItem]
    }
    deriving (Eq, Show)

data Trans
    = TransWithOps [TransOp] StateNum
    | TransReduce LAPEG.AltNum
    deriving (Eq, Show)

data TransOp
    = TransOpEnter LAPEG.VarNum Bool (Maybe StateNum)
    | TransOpPushBackpoint StateNum
    | TransOpHandleNot LAPEG.AltNum
    | TransOpShift
    deriving (Eq, Show, Generic)

instance Hashable TransOp

data AltItem = AltItem
    {
        altItemAltNum :: LAPEG.AltNum,
        altItemCurPos :: LAPEG.Position
    }
    deriving (Eq, Show)
