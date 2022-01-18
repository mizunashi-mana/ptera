module Language.Parser.Ptera.Machine.SRB where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Symbolic.IntMap as SymbolicIntMap
import qualified Language.Parser.Ptera.Machine.PEG        as PEG


type T = SRB

data SRB start varDoc altDoc a = SRB
    { initials    :: EnumMap.EnumMap start StateNum
    , states      :: AlignableArray.T StateNum MState
    , alts        :: AlignableArray.T PEG.AltNum (PEG.Alt altDoc a)
    , vars :: AlignableArray.T PEG.VarNum (PEG.Var varDoc)
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
    | TransReduce PEG.AltNum
    deriving (Eq, Show)

data TransOp
    = TransOpEnter PEG.VarNum Bool (Maybe StateNum)
    | TransOpPushBackpoint StateNum
    | TransOpHandleNot PEG.AltNum
    | TransOpShift
    deriving (Eq, Show, Generic)

instance Hashable TransOp

data AltItem = AltItem
    {
        altItemAltNum :: PEG.AltNum,
        altItemCurPos :: PEG.Position
    }
    deriving (Eq, Show)
