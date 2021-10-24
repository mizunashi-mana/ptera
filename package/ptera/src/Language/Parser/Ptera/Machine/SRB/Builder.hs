module Language.Parser.Ptera.Machine.SRB.Builder where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Alignable.Map   as AlignableMap
import qualified Language.Parser.Ptera.Machine.PEG          as PEG
import qualified Language.Parser.Ptera.Machine.SRB          as SRB


type T a = BuilderT a

type BuilderT a = StateT (Context a)

data Context a = Context
    {
        ctxInitials       :: [(PEG.StartPoint, SRB.StateNum)],
        ctxNextStateNum   :: SRB.StateNum,
        ctxStates         :: AlignableMap.T SRB.StateNum SRB.MState,
        ctxNextRuleAltNum :: SRB.RuleAltNum,
        ctxRuleItems      :: AlignableMap.T SRB.RuleAltNum (SRB.RuleAlt a)
    }
    deriving (Eq, Show)

build :: Monad m => BuilderT a m () -> m (SRB.T a)
build builder = do
    finalCtx <- execStateT builder initialCtx
    pure do
        SRB.SRB
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

getNewStateNum :: Monad m => BuilderT a m SRB.StateNum
getNewStateNum = do
    ctx <- get
    let sn = ctxNextStateNum ctx
    put do ctx { ctxNextStateNum = Alignable.nextAlign sn }
    pure sn

registerInitial :: Monad m => PEG.StartPoint -> SRB.StateNum -> BuilderT a m ()
registerInitial i v = modify' \ctx -> ctx
    {
        ctxInitials = (i, v):ctxInitials ctx
    }

addRuleAlt :: Monad m => SRB.RuleAlt a -> BuilderT a m SRB.RuleAltNum
addRuleAlt alt = do
    ctx <- get
    let n = ctxNextRuleAltNum ctx
    put do ctx
            { ctxNextRuleAltNum = Alignable.nextAlign n
            , ctxRuleItems = AlignableMap.insert n alt
                do ctxRuleItems ctx
            }
    pure n

addState :: Monad m => SRB.MState -> BuilderT a m ()
addState s = modify' \ctx -> ctx
    {
        ctxStates = AlignableMap.insert
            do SRB.stateNum s
            do s
            do ctxStates ctx
    }
