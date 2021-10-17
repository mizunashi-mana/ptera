module Language.Parser.Ptera.Machine.SSRB where

import           Language.Parser.Ptera.Prelude

import qualified Data.Array                                 as Array
import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Data.IntMap.Strict                         as IntMap
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Machine.PEG          as PEG


type T = SSRB

data SSRB a = SSRB
    {
        initials :: EnumMap.EnumMap PEG.StartPoint StateNum,
        states   :: AlignableArray.T StateNum MState,
        ruleAlts :: AlignableArray.T RuleAltNum (RuleAlt a)
    }
    deriving (Eq, Show)

newtype StateNum = StateNum Int
    deriving (Eq, Show)
    deriving Alignable.T via Alignable.Inst

data MState = MState
    {
        stateNum      :: StateNum,
        stateTrans    :: Trans,
        stateRuleItem :: RuleItem
    }
    deriving (Eq, Show)

data Trans
    = TransShift Int StateNum
    | TransEnter PEG.Var StateNum StateNum
    | TransPushBackpoint StateNum StateNum
    | TransReduce RuleAltNum
    | TransReduceNot RuleAltNum
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
    deriving (Eq, Show)
