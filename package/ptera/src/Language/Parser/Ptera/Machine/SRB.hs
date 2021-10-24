module Language.Parser.Ptera.Machine.SRB where

import           Language.Parser.Ptera.Prelude

import qualified Data.Array                                 as Array
import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Symbolic.IntMap as SymbolicIntMap
import qualified Language.Parser.Ptera.Machine.PEG          as PEG


type T = SRB

data SRB a = SRB
    {
        initials :: EnumMap.EnumMap PEG.StartPoint StateNum,
        states   :: AlignableArray.T StateNum MState,
        ruleAlts :: AlignableArray.T RuleAltNum (RuleAlt a)
    }
    deriving (Eq, Show, Functor)

newtype StateNum = StateNum Int
    deriving (Eq, Show)
    deriving Alignable.T via Alignable.Inst

data MState = MState
    {
        stateNum      :: StateNum,
        stateTrans    :: SymbolicIntMap.T Trans,
        stateRuleItem :: RuleItem
    }
    deriving (Eq, Show)

data Trans
    = TransWithOps [TransOp] StateNum
    | TransReduce RuleAltNum
    | TransReduceNot RuleAltNum
    deriving (Eq, Show)

data TransOp
    = TransOpEnter PEG.Var StateNum
    | TransOpPushBackpoint StateNum
    | TransOpShift
    deriving (Eq, Show)

data RuleItem = RuleItem
    {
        ruleItemAltNum :: RuleAltNum,
        ruleItemCurPos :: Int
    }
    deriving (Eq, Show)

newtype RuleAltNum = RuleAltNum Int
    deriving (Eq, Show)
    deriving Alignable.T via Alignable.Inst

data RuleAlt a = RuleAlt
    {
        ruleAltVar        :: PEG.Var,
        ruleAltSeq        :: Array.Array Int PEG.Unit,
        ruleAltKind       :: PEG.AltKind,
        ruleWithBackpoint :: Bool,
        ruleAltAction     :: a
    }
    deriving (Eq, Show, Functor)
