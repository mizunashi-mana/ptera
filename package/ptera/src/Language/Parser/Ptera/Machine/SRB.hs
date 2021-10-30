module Language.Parser.Ptera.Machine.SRB where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Symbolic.IntMap as SymbolicIntMap
import qualified Language.Parser.Ptera.Machine.PEG        as PEG
import qualified Language.Parser.Ptera.Machine.LAPEG        as LAPEG


type T = SRB

data SRB a = SRB
    {
        initials :: EnumMap.EnumMap PEG.StartPoint StateNum,
        states   :: AlignableArray.T StateNum MState,
        alts :: AlignableArray.T LAPEG.AltNum (LAPEG.Alt a)
    }
    deriving (Eq, Show, Functor)

newtype StateNum = StateNum Int
    deriving (Eq, Show)
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
    | TransReduceNot LAPEG.AltNum
    deriving (Eq, Show)

data TransOp
    = TransOpEnter LAPEG.Var (Maybe StateNum)
    | TransOpPushBackpoint StateNum
    | TransOpShift
    deriving (Eq, Show)

data AltItem = AltItem
    {
        altItemAltNum :: LAPEG.AltNum,
        altItemCurPos :: LAPEG.Position
    }
    deriving (Eq, Show)
