module Language.Parser.Ptera.Machine.SRB.Builder where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Alignable.Map   as AlignableMap
import qualified Language.Parser.Ptera.Machine.LAPEG        as LAPEG
import qualified Language.Parser.Ptera.Machine.SRB          as SRB


type T s a = BuilderT s a

type BuilderT s a = StateT (Context s a)

data Context s a = Context
    {
        ctxInitials     :: EnumMap.EnumMap s SRB.StateNum,
        ctxNextStateNum :: SRB.StateNum,
        ctxStates       :: AlignableMap.T SRB.StateNum SRB.MState
    }
    deriving (Eq, Show)

type Alts a = AlignableArray.T LAPEG.AltNum (LAPEG.Alt a)

build :: Monad m => Alts a -> BuilderT s a m () -> m (SRB.T s a)
build alts builder = do
    finalCtx <- execStateT builder initialCtx
    pure do
        SRB.SRB
            { initials = ctxInitials finalCtx
            , states = AlignableArray.fromTotalMap
                do ctxNextStateNum finalCtx
                do ctxStates finalCtx
            , alts = alts
            }
    where
        initialCtx = Context
            {
                ctxInitials = EnumMap.empty,
                ctxNextStateNum = Alignable.initialAlign,
                ctxStates = AlignableMap.empty
            }

genNewStateNum :: Monad m => BuilderT s a m SRB.StateNum
genNewStateNum = do
    ctx <- get
    let sn = ctxNextStateNum ctx
    put do ctx { ctxNextStateNum = Alignable.nextAlign sn }
    pure sn

registerInitial :: Monad m => Enum s => s -> SRB.StateNum -> BuilderT s a m ()
registerInitial i v = modify' \ctx -> ctx
    {
        ctxInitials = EnumMap.insert i v do ctxInitials ctx
    }

addState :: Monad m => SRB.MState -> BuilderT s a m ()
addState s = modify' \ctx -> ctx
    {
        ctxStates = AlignableMap.insert
            do SRB.stateNum s
            do s
            do ctxStates ctx
    }
