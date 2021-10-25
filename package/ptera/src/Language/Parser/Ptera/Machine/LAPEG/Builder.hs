module Language.Parser.Ptera.Machine.LAPEG.Builder where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Alignable.Map   as AlignableMap
import qualified Language.Parser.Ptera.Machine.LAPEG        as LAPEG
import qualified Language.Parser.Ptera.Machine.PEG          as PEG


type T a = BuilderT a

type BuilderT a = StateT (Context a)

data Context a = Context
    {
        ctxInitials :: [(PEG.StartPoint, LAPEG.Var)],
        ctxNextAltNum :: LAPEG.AltNum,
        ctxAlts :: AlignableMap.T LAPEG.AltNum (LAPEG.Alt a),
        ctxNextVar  :: LAPEG.Var,
        ctxRules    :: AlignableMap.T LAPEG.Var LAPEG.Rule
    }
    deriving (Eq, Show)

build :: Monad m => BuilderT a m () -> m (LAPEG.T a)
build builder = do
    finalCtx <- execStateT builder initialCtx
    pure do
        LAPEG.LAPEG
            { initials = EnumMap.fromList do ctxInitials finalCtx
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
                ctxInitials = [],
                ctxNextAltNum = Alignable.initialAlign,
                ctxAlts = AlignableMap.empty,
                ctxNextVar = Alignable.initialAlign,
                ctxRules = AlignableMap.empty
            }

genNewAltNum :: Monad m => BuilderT a m LAPEG.AltNum
genNewAltNum = do
    ctx <- get
    let n = ctxNextAltNum ctx
    put do ctx { ctxNextAltNum = Alignable.nextAlign n }
    pure n

genNewVar :: Monad m => BuilderT a m LAPEG.Var
genNewVar = do
    ctx <- get
    let v = ctxNextVar ctx
    put do ctx { ctxNextVar = Alignable.nextAlign v }
    pure v

registerInitial :: Monad m => PEG.StartPoint -> LAPEG.Var -> BuilderT a m ()
registerInitial i v = modify' \ctx -> ctx
    {
        ctxInitials = (i, v):ctxInitials ctx
    }

addAlt :: Monad m => LAPEG.Alt a -> BuilderT a m LAPEG.AltNum
addAlt alt = do
    n <- genNewAltNum
    modify' \ctx -> ctx
        {
            ctxAlts = AlignableMap.insert n alt
                do ctxAlts ctx
        }
    pure n

addRule :: Monad m => LAPEG.Var -> LAPEG.Rule -> BuilderT a m ()
addRule v e = modify' \ctx -> ctx
    {
        ctxRules = AlignableMap.insert v e
            do ctxRules ctx
    }
