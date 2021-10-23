module Language.Parser.Ptera.Machine.LAPEG.Builder where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Alignable.Map   as AlignableMap
import qualified Language.Parser.Ptera.Machine.PEG          as PEG
import qualified Language.Parser.Ptera.Machine.LAPEG        as LAPEG


type T a = BuilderT a

type BuilderT a = StateT (Context a)

data Context a = Context
    {
        ctxInitials :: [(PEG.StartPoint, LAPEG.Var)],
        ctxNextVar :: LAPEG.Var,
        ctxRules :: AlignableMap.T LAPEG.Var (LAPEG.LAPE a)
    }
    deriving (Eq, Show)

build :: Monad m => BuilderT a m () -> m (LAPEG.T a)
build builder = do
    finalCtx <- finalCtxM
    pure do
        LAPEG.LAPEG
            { initials = EnumMap.fromList do ctxInitials finalCtx
            , rules = AlignableArray.fromMap
                do ctxNextVar finalCtx
                do ctxRules finalCtx
            }
    where
        initialCtx = Context
            {
                ctxInitials = [],
                ctxNextVar = Alignable.initialAlign,
                ctxRules = AlignableMap.empty
            }

        finalCtxM = execStateT builder initialCtx

getNewVar :: Monad m => BuilderT a m LAPEG.Var
getNewVar = do
    ctx <- get
    let v = ctxNextVar ctx
    put do ctx { ctxNextVar = Alignable.nextAlign v }
    pure v

registerInitial :: Monad m => PEG.StartPoint -> LAPEG.Var -> BuilderT a m ()
registerInitial i v = modify' \ctx -> ctx
    {
        ctxInitials = (i, v):ctxInitials ctx
    }

addRule :: Monad m => LAPEG.Var -> LAPEG.LAPE a -> BuilderT a m ()
addRule v e = modify' \ctx -> ctx
    {
        ctxRules = AlignableMap.insert v e
            do ctxRules ctx
    }
