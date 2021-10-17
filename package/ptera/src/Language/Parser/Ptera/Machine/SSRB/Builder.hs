module Language.Parser.Ptera.Machine.SSRB.Builder where

import           Language.Parser.Ptera.Prelude

import qualified Data.Array                                 as Array
import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Data.IntMap.Strict                         as IntMap
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Alignable.Map   as AlignableMap
import qualified Language.Parser.Ptera.Machine.PEG          as PEG
import qualified Language.Parser.Ptera.Machine.SSRB         as SSRB


type T a = Builder a

type Builder a = State (BuilderContext a)

data BuilderContext a = BuilderContext
    {
        builderCtxInitials :: [(PEG.StartPoint, SSRB.StateNum)],
        builderCtxNextStateNum :: SSRB.StateNum,
        builderCtxStates :: AlignableMap.T SSRB.StateNum SSRB.MState,
        builderCtxNextRuleAltNum :: SSRB.RuleAltNum,
        builderCtxRuleItems :: AlignableMap.T SSRB.RuleAltNum (SSRB.RuleAlt a)
    }
    deriving (Eq, Show)

build :: Builder a () -> SSRB.T a
build builder = SSRB.SSRB
    { initials = EnumMap.fromList do builderCtxInitials finalCtx
    , states = AlignableArray.fromMap
        do builderCtxNextStateNum finalCtx
        do builderCtxStates finalCtx
    , ruleAlts = AlignableArray.fromMap
        do builderCtxNextRuleAltNum finalCtx
        do builderCtxRuleItems finalCtx
    }
    where
        initialCtx = BuilderContext
            {
                builderCtxInitials = [],
                builderCtxNextStateNum = Alignable.initialAlign,
                builderCtxStates = AlignableMap.empty,
                builderCtxNextRuleAltNum = Alignable.initialAlign,
                builderCtxRuleItems = AlignableMap.empty
            }

        finalCtx = execState builder initialCtx

getNewStateNum :: Builder a SSRB.StateNum
getNewStateNum = do
    ctx <- get
    let sn = builderCtxNextStateNum ctx
    put do ctx { builderCtxNextStateNum = Alignable.nextAlign sn }
    pure sn

registerInitial :: PEG.StartPoint -> SSRB.StateNum -> Builder a ()
registerInitial i v = modify' \ctx -> ctx
    {
        builderCtxInitials = (i, v):builderCtxInitials ctx
    }

addRuleAlt :: SSRB.RuleAlt a -> Builder a SSRB.RuleAltNum
addRuleAlt alt = do
    ctx <- get
    let n = builderCtxNextRuleAltNum ctx
    put do ctx
            { builderCtxNextRuleAltNum = Alignable.nextAlign n
            , builderCtxRuleItems = AlignableMap.insert n alt
                do builderCtxRuleItems ctx
            }
    pure n

addState :: SSRB.MState -> Builder a ()
addState s = modify' \ctx -> ctx
    {
        builderCtxStates = AlignableMap.insert
            do SSRB.stateNum s
            do s
            do builderCtxStates ctx
    }
