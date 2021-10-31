module Language.Parser.Ptera.Machine.LAPEG.Builder where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Alignable.Map   as AlignableMap
import qualified Language.Parser.Ptera.Machine.LAPEG        as LAPEG


type T s a = BuilderT s a

type BuilderT s a = StateT (Context s a)

data Context s a = Context
    {
        ctxInitials   :: EnumMap.EnumMap s LAPEG.Var,
        ctxNextAltNum :: LAPEG.AltNum,
        ctxAlts       :: AlignableMap.T LAPEG.AltNum (LAPEG.Alt a),
        ctxNextVar    :: LAPEG.Var,
        ctxRules      :: AlignableMap.T LAPEG.Var LAPEG.Rule
    }
    deriving (Eq, Show)

build :: Monad m => BuilderT s a m () -> m (LAPEG.T s a)
build builder = do
    finalCtx <- execStateT builder initialCtx
    pure do
        LAPEG.LAPEG
            { initials = ctxInitials finalCtx
            , alts = AlignableArray.fromTotalMap
                do ctxNextAltNum finalCtx
                do ctxAlts finalCtx
            , rules = AlignableArray.fromTotalMap
                do ctxNextVar finalCtx
                do ctxRules finalCtx
            }
    where
        initialCtx = Context
            {
                ctxInitials = EnumMap.empty,
                ctxNextAltNum = Alignable.initialAlign,
                ctxAlts = AlignableMap.empty,
                ctxNextVar = Alignable.initialAlign,
                ctxRules = AlignableMap.empty
            }

genNewAltNum :: Monad m => BuilderT s a m LAPEG.AltNum
genNewAltNum = do
    ctx <- get
    let n = ctxNextAltNum ctx
    put do ctx { ctxNextAltNum = Alignable.nextAlign n }
    pure n

genNewVar :: Monad m => BuilderT s a m LAPEG.Var
genNewVar = do
    ctx <- get
    let v = ctxNextVar ctx
    put do ctx { ctxNextVar = Alignable.nextAlign v }
    pure v

registerInitial :: Monad m => Enum s => s -> LAPEG.Var -> BuilderT s a m ()
registerInitial i v = modify' \ctx -> ctx
    {
        ctxInitials = EnumMap.insert i v do ctxInitials ctx
    }

addAlt :: Monad m => LAPEG.Alt a -> BuilderT s a m LAPEG.AltNum
addAlt alt = do
    n <- genNewAltNum
    modify' \ctx -> ctx
        {
            ctxAlts = AlignableMap.insert n alt
                do ctxAlts ctx
        }
    pure n

addRule :: Monad m => LAPEG.Var -> LAPEG.Rule -> BuilderT s a m ()
addRule v e = modify' \ctx -> ctx
    {
        ctxRules = AlignableMap.insert v e
            do ctxRules ctx
    }
