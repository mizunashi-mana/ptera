module Language.Parser.Ptera.Machine.PEG.Builder where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Alignable.Map   as AlignableMap
import qualified Language.Parser.Ptera.Machine.PEG          as PEG


type T start doc a = BuilderT start doc a

type BuilderT start doc a = StateT (Context start doc a)

data Context start doc a = Context
    {
        ctxInitials    :: EnumMap.EnumMap start PEG.Var,
        ctxNextVar     :: PEG.Var,
        ctxRules       :: AlignableMap.T PEG.Var (PEG.Rule a),
        ctxDisplayVars :: AlignableMap.T PEG.Var doc
    }
    deriving (Eq, Show)

build :: Monad m => BuilderT start doc a m () -> m (PEG.T start doc a)
build builder = do
    finalCtx <- execStateT builder initialCtx
    pure do
        PEG.PEG
            { initials = ctxInitials finalCtx
            , rules = AlignableArray.fromTotalMap
                do ctxNextVar finalCtx
                do ctxRules finalCtx
            , displayVars = AlignableArray.fromTotalMap
                do ctxNextVar finalCtx
                do ctxDisplayVars finalCtx
            }
    where
        initialCtx = Context
            {
                ctxInitials = EnumMap.empty,
                ctxNextVar = Alignable.initialAlign,
                ctxRules = AlignableMap.empty,
                ctxDisplayVars = AlignableMap.empty
            }

genNewVar :: Monad m => doc -> BuilderT start doc a m PEG.Var
genNewVar d = do
    v <- ctxNextVar <$> get
    modify' \ctx -> ctx
        { ctxNextVar = Alignable.nextAlign v
        , ctxDisplayVars = AlignableMap.insert v d
            do ctxDisplayVars ctx
        }
    pure v

addInitial :: Monad m => Enum start => start -> PEG.Var -> BuilderT start doc a m ()
addInitial i v = modify' \ctx -> ctx
    {
        ctxInitials = EnumMap.insert i v do ctxInitials ctx
    }

addRule :: Monad m => PEG.Var -> PEG.Rule a -> BuilderT start doc a m ()
addRule v e = modify' \ctx -> ctx
    { ctxRules = AlignableMap.insert v e
        do ctxRules ctx
    }
