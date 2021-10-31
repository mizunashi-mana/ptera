module Language.Parser.Ptera.Machine.PEG.Builder where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Alignable.Map   as AlignableMap
import qualified Language.Parser.Ptera.Machine.PEG          as PEG


type T s a = BuilderT s a

type BuilderT s a = StateT (Context s a)

data Context s a = Context
    {
        ctxInitials :: EnumMap.EnumMap s PEG.Var,
        ctxNextVar  :: PEG.Var,
        ctxRules    :: AlignableMap.T PEG.Var (PEG.Rule a)
    }
    deriving (Eq, Show)

build :: Monad m => BuilderT s a m () -> m (PEG.T s a)
build builder = do
    finalCtx <- execStateT builder initialCtx
    pure do
        PEG.PEG
            { initials = ctxInitials finalCtx
            , rules = AlignableArray.fromTotalMap
                do ctxNextVar finalCtx
                do ctxRules finalCtx
            }
    where
        initialCtx = Context
            {
                ctxInitials = EnumMap.empty,
                ctxNextVar = Alignable.initialAlign,
                ctxRules = AlignableMap.empty
            }

genNewVar :: Monad m => BuilderT s a m PEG.Var
genNewVar = do
    ctx <- get
    let v = ctxNextVar ctx
    put do ctx { ctxNextVar = Alignable.nextAlign v }
    pure v

addInitial :: Monad m => Enum s => s -> PEG.Var -> BuilderT s a m ()
addInitial i v = modify' \ctx -> ctx
    {
        ctxInitials = EnumMap.insert i v do ctxInitials ctx
    }

addRule :: Monad m => PEG.Var -> PEG.Rule a -> BuilderT s a m ()
addRule v e = modify' \ctx -> ctx
    {
        ctxRules = AlignableMap.insert v e
            do ctxRules ctx
    }
