module Language.Parser.Ptera.Machine.SRB.Builder where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Alignable.Map   as AlignableMap
import qualified Language.Parser.Ptera.Machine.PEG          as PEG
import qualified Language.Parser.Ptera.Machine.LAPEG          as LAPEG
import qualified Language.Parser.Ptera.Machine.SRB          as SRB


type T a = BuilderT a

type BuilderT a = StateT (Context a)

data Context a = Context
    {
        ctxInitials       :: [(PEG.StartPoint, SRB.StateNum)],
        ctxNextStateNum   :: SRB.StateNum,
        ctxStates         :: AlignableMap.T SRB.StateNum SRB.MState
    }
    deriving (Eq, Show)

type Alts a = AlignableArray.T LAPEG.AltNum (LAPEG.Alt a)

build :: Monad m => Alts a -> BuilderT a m () -> m (SRB.T a)
build alts builder = do
    finalCtx <- execStateT builder initialCtx
    pure do
        SRB.SRB
            { initials = EnumMap.fromList do ctxInitials finalCtx
            , states = AlignableArray.fromTotalMap
                do ctxNextStateNum finalCtx
                do ctxStates finalCtx
            , alts = alts
            }
    where
        initialCtx = Context
            {
                ctxInitials = [],
                ctxNextStateNum = Alignable.initialAlign,
                ctxStates = AlignableMap.empty
            }

genNewStateNum :: Monad m => BuilderT a m SRB.StateNum
genNewStateNum = do
    ctx <- get
    let sn = ctxNextStateNum ctx
    put do ctx { ctxNextStateNum = Alignable.nextAlign sn }
    pure sn

registerInitial :: Monad m => PEG.StartPoint -> SRB.StateNum -> BuilderT a m ()
registerInitial i v = modify' \ctx -> ctx
    {
        ctxInitials = (i, v):ctxInitials ctx
    }

addState :: Monad m => SRB.MState -> BuilderT a m ()
addState s = modify' \ctx -> ctx
    {
        ctxStates = AlignableMap.insert
            do SRB.stateNum s
            do s
            do ctxStates ctx
    }
