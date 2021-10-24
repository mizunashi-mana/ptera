module Language.Parser.Ptera.Machine.SRB.Builder where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Alignable.Map   as AlignableMap
import qualified Language.Parser.Ptera.Machine.PEG          as PEG
import qualified Language.Parser.Ptera.Machine.SRB         as SRB


type T a = Builder a

type Builder a = State (Context a)

data Context a = Context
    {
        ctxInitials :: [(PEG.StartPoint, SRB.StateNum)],
        ctxNextStateNum :: SRB.StateNum,
        ctxStates :: AlignableMap.T SRB.StateNum SRB.MState,
        ctxNextRuleAltNum :: SRB.RuleAltNum,
        ctxRuleItems :: AlignableMap.T SRB.RuleAltNum (SRB.RuleAlt a)
    }
    deriving (Eq, Show)

build :: Builder a () -> SRB.T a
build builder = SRB.SRB
    { initials = EnumMap.fromList do ctxInitials finalCtx
    , states = AlignableArray.fromMap
        do ctxNextStateNum finalCtx
        do ctxStates finalCtx
    , ruleAlts = AlignableArray.fromMap
        do ctxNextRuleAltNum finalCtx
        do ctxRuleItems finalCtx
    }
    where
        initialCtx = Context
            {
                ctxInitials = [],
                ctxNextStateNum = Alignable.initialAlign,
                ctxStates = AlignableMap.empty,
                ctxNextRuleAltNum = Alignable.initialAlign,
                ctxRuleItems = AlignableMap.empty
            }

        finalCtx = execState builder initialCtx

getNewStateNum :: Builder a SRB.StateNum
getNewStateNum = do
    ctx <- get
    let sn = ctxNextStateNum ctx
    put do ctx { ctxNextStateNum = Alignable.nextAlign sn }
    pure sn

registerInitial :: PEG.StartPoint -> SRB.StateNum -> Builder a ()
registerInitial i v = modify' \ctx -> ctx
    {
        ctxInitials = (i, v):ctxInitials ctx
    }

addRuleAlt :: SRB.RuleAlt a -> Builder a SRB.RuleAltNum
addRuleAlt alt = do
    ctx <- get
    let n = ctxNextRuleAltNum ctx
    put do ctx
            { ctxNextRuleAltNum = Alignable.nextAlign n
            , ctxRuleItems = AlignableMap.insert n alt
                do ctxRuleItems ctx
            }
    pure n

addState :: SRB.MState -> Builder a ()
addState s = modify' \ctx -> ctx
    {
        ctxStates = AlignableMap.insert
            do SRB.stateNum s
            do s
            do ctxStates ctx
    }
