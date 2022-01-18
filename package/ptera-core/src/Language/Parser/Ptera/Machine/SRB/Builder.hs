module Language.Parser.Ptera.Machine.SRB.Builder where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Alignable.Map   as AlignableMap
import qualified Language.Parser.Ptera.Machine.PEG        as PEG
import qualified Language.Parser.Ptera.Machine.SRB          as SRB


type T start a = BuilderT start a

type BuilderT start a = StateT (Context start a)

data Context start a = Context
    {
        ctxInitials     :: EnumMap.EnumMap start SRB.StateNum,
        ctxNextStateNum :: SRB.StateNum,
        ctxStates       :: AlignableMap.T SRB.StateNum SRB.MState
    }
    deriving (Eq, Show)

type Vars varDoc = AlignableArray.T PEG.VarNum (PEG.Var varDoc)
type Alts altDoc a = AlignableArray.T PEG.AltNum (PEG.Alt altDoc a)

build :: Monad m
    => Vars varDoc -> Alts altDoc a -> BuilderT start a m ()
    -> m (SRB.T start varDoc altDoc a)
build vars alts builder = do
    finalCtx <- execStateT builder initialCtx
    pure do
        SRB.SRB
            { initials = ctxInitials finalCtx
            , states = AlignableArray.fromTotalMap
                do ctxNextStateNum finalCtx
                do ctxStates finalCtx
            , alts = alts
            , vars = vars
            }
    where
        initialCtx = Context
            {
                ctxInitials = EnumMap.empty,
                ctxNextStateNum = Alignable.initialAlign,
                ctxStates = AlignableMap.empty
            }

genNewStateNum :: Monad m => BuilderT start a m SRB.StateNum
genNewStateNum = do
    ctx <- get
    let sn = ctxNextStateNum ctx
    put do ctx { ctxNextStateNum = Alignable.nextAlign sn }
    pure sn

registerInitial :: Monad m => Enum start
    => start -> SRB.StateNum -> BuilderT start a m ()
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
