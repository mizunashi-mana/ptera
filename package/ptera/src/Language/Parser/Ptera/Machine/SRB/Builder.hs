module Language.Parser.Ptera.Machine.SRB.Builder where

import Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict as EnumMap
import qualified Language.Parser.Ptera.Machine.PEG as PEG
import qualified Language.Parser.Ptera.Data.Alignable as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Alignable.Map as AlignableMap
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Array as Array
import qualified Language.Parser.Ptera.Machine.SRB as SRB


type T a = SRBBuilder a

type SRBBuilder a = State (SRBBuilderContext a)

data SRBBuilderContext a = SRBBuilderContext
    {
        srbBuilderCtxInitials :: [(PEG.StartPoint, SRB.StateNum)],
        srbBuilderCtxNextStateNum :: SRB.StateNum,
        srbBuilderCtxTrans :: AlignableMap.T SRB.StateNum SRB.SRBState,
        srbBuilderCtxNextRuleAltNum :: SRB.RuleAltNum,
        srbBuilderCtxRuleItems :: AlignableMap.T SRB.RuleAltNum (SRB.RuleAlt a)
    }
    deriving (Eq, Show)

buildSRB :: SRBBuilder a () -> SRB.T a
buildSRB builder = SRB.SRB
    { srbInitials = EnumMap.fromList do srbBuilderCtxInitials finalCtx
    , srbTrans = AlignableArray.fromMap
        do srbBuilderCtxNextStateNum finalCtx
        do srbBuilderCtxTrans finalCtx
    , srbRuleAlts = AlignableArray.fromMap
        do srbBuilderCtxNextRuleAltNum finalCtx
        do srbBuilderCtxRuleItems finalCtx
    }
    where
        initialCtx = SRBBuilderContext
            {
                srbBuilderCtxInitials = [],
                srbBuilderCtxNextStateNum = Alignable.initialAlign,
                srbBuilderCtxTrans = AlignableMap.empty,
                srbBuilderCtxNextRuleAltNum = Alignable.initialAlign,
                srbBuilderCtxRuleItems = AlignableMap.empty
            }

        finalCtx = execState builder initialCtx


