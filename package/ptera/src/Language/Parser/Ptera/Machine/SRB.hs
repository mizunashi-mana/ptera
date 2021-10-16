module Language.Parser.Ptera.Machine.SRB where

import Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict as EnumMap
import qualified Language.Parser.Ptera.Machine.PEG as PEG
import qualified Language.Parser.Ptera.Data.Alignable as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Array as Array


type T = SRB

data SRB a = SRB
    {
        srbInitials :: EnumMap.EnumMap PEG.StartPoint StateNum,
        srbTrans :: AlignableArray.T StateNum SRBState,
        srbRuleAlts :: AlignableArray.T RuleAltNum (RuleAlt a)
    }
    deriving (Eq, Show)

newtype StateNum = StateNum Int
    deriving (Eq, Show)
    deriving Alignable.T via Alignable.Inst

data SRBState = SRBState
    {
        srbStateNum :: StateNum,
        srbStateTrans :: IntMap.IntMap SRBTrans,
        srbStateTransOther :: Maybe SRBTrans,
        srbStateRuleItem :: RuleItem
    }
    deriving (Eq, Show)

data SRBTrans = SRBTrans
    {
        srbTransOps :: [SRBOp],
        srbTransStateNum :: StateNum
    }
    deriving (Eq, Show)

data SRBOp
    = SRBOpShift
    | SRBOpEnter PEG.Var StateNum
    | SRBOpPushBackpoint StateNum
    | SRBOpReduce RuleAltNum
    | SRBOpReduceNot RuleAltNum
    | SRBOpAccept
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
        ruleAltVar :: PEG.Var,
        ruleAltSeq :: UnitSeq,
        ruleAltKind :: PEG.AltKind,
        ruleWithBackpoint :: Bool,
        ruleAltAction :: a
    }
    deriving (Eq, Show)

newtype UnitSeq = UnitSeq (Array.Array Int PEG.Unit)
    deriving (Eq, Show)
