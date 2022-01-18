module Language.Parser.Ptera.Machine.LAPEG.Builder where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Alignable.Map   as AlignableMap
import qualified Language.Parser.Ptera.Machine.PEG        as PEG
import qualified Language.Parser.Ptera.Machine.LAPEG        as LAPEG


type T start varDoc altDoc a = BuilderT start varDoc altDoc a

type BuilderT start varDoc altDoc a = StateT (Context start varDoc altDoc a)

data Context start varDoc altDoc a = Context
    { ctxInitials    :: EnumMap.EnumMap start LAPEG.VarNum
    , ctxNextAltNum  :: LAPEG.AltNum
    , ctxNextVar     :: LAPEG.VarNum
    , ctxRules       :: AlignableMap.T LAPEG.VarNum LAPEG.Rule
    , ctxVars        :: AlignableMap.T LAPEG.VarNum (PEG.Var varDoc)
    , ctxAlts        :: AlignableMap.T LAPEG.AltNum (LAPEG.Alt altDoc a)
    }
    deriving (Eq, Show, Functor)

build :: Monad m
    => BuilderT start varDoc altDoc a m () -> m (LAPEG.T start varDoc altDoc a)
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
            , vars = AlignableArray.fromTotalMap
                do ctxNextVar finalCtx
                do ctxVars finalCtx
            }
    where
        initialCtx = Context
            { ctxInitials = EnumMap.empty
            , ctxNextAltNum = Alignable.initialAlign
            , ctxAlts = AlignableMap.empty
            , ctxNextVar = Alignable.initialAlign
            , ctxRules = AlignableMap.empty
            , ctxVars = AlignableMap.empty
            }

genNewAltNum :: Monad m => BuilderT start varDoc altDoc a m LAPEG.AltNum
genNewAltNum = do
    ctx <- get
    let n = ctxNextAltNum ctx
    put do ctx { ctxNextAltNum = Alignable.nextAlign n }
    pure n

genNewVar :: Monad m
    => PEG.Var varDoc -> BuilderT start varDoc altDoc a m LAPEG.VarNum
genNewVar v = do
    vn <- ctxNextVar <$> get
    modify' \ctx -> ctx
        { ctxNextVar = Alignable.nextAlign vn
        , ctxVars = AlignableMap.insert vn v
            do ctxVars ctx
        }
    pure vn

registerInitial :: Monad m => Enum start
    => start -> LAPEG.VarNum -> BuilderT start varDoc altDoc a m ()
registerInitial i v = modify' \ctx -> ctx
    {
        ctxInitials = EnumMap.insert i v do ctxInitials ctx
    }

addAlt :: Monad m
    => LAPEG.Alt altDoc a -> BuilderT start varDoc altDoc a m LAPEG.AltNum
addAlt alt = do
    n <- genNewAltNum
    modify' \ctx -> ctx
        {
            ctxAlts = AlignableMap.insert n alt
                do ctxAlts ctx
        }
    pure n

addRule :: Monad m
    => LAPEG.VarNum -> LAPEG.Rule -> BuilderT start varDoc altDoc a m ()
addRule v e = modify' \ctx -> ctx
    { ctxRules = AlignableMap.insert v e
        do ctxRules ctx
    }
