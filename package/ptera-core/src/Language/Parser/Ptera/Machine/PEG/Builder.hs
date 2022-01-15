module Language.Parser.Ptera.Machine.PEG.Builder where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Alignable.Map   as AlignableMap
import qualified Language.Parser.Ptera.Machine.PEG          as PEG


type T start varDoc altDoc a = BuilderT start varDoc altDoc a

type BuilderT start varDoc altDoc a = StateT (Context start varDoc altDoc a)

data Context start varDoc altDoc a = Context
    { ctxInitials    :: EnumMap.EnumMap start PEG.VarNum
    , ctxNextVar     :: PEG.VarNum
    , ctxRules       :: AlignableMap.T PEG.VarNum (PEG.Rule altDoc a)
    , ctxVars        :: AlignableMap.T PEG.VarNum (PEG.Var varDoc)
    }
    deriving (Eq, Show)

build :: Monad m
    => BuilderT start varDoc altDoc a m () -> m (PEG.T start varDoc altDoc a)
build builder = do
    finalCtx <- execStateT builder initialCtx
    pure do
        PEG.PEG
            { initials = ctxInitials finalCtx
            , rules = AlignableArray.fromTotalMap
                do ctxNextVar finalCtx
                do ctxRules finalCtx
            , vars = AlignableArray.fromTotalMap
                do ctxNextVar finalCtx
                do ctxVars finalCtx
            }
    where
        initialCtx = Context
            {
                ctxInitials = EnumMap.empty,
                ctxNextVar = Alignable.initialAlign,
                ctxRules = AlignableMap.empty,
                ctxVars = AlignableMap.empty
            }

genNewVar :: Monad m
    => PEG.Var varDoc -> BuilderT start varDoc altDoc a m PEG.VarNum
genNewVar v = do
    vn <- ctxNextVar <$> get
    modify' \ctx -> ctx
        { ctxNextVar = Alignable.nextAlign vn
        , ctxVars = AlignableMap.insert vn v
            do ctxVars ctx
        }
    pure vn

addInitial :: Monad m => Enum start
    => start -> PEG.VarNum -> BuilderT start varDoc altDoc a m ()
addInitial i v = modify' \ctx -> ctx
    {
        ctxInitials = EnumMap.insert i v do ctxInitials ctx
    }

addRule :: Monad m
    => PEG.VarNum -> PEG.Rule altDoc a -> BuilderT start varDoc altDoc a m ()
addRule v e = modify' \ctx -> ctx
    { ctxRules = AlignableMap.insert v e
        do ctxRules ctx
    }
