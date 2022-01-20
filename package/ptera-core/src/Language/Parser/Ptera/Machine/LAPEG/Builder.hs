module Language.Parser.Ptera.Machine.LAPEG.Builder where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Alignable.Map   as AlignableMap
import qualified Language.Parser.Ptera.Machine.PEG          as PEG
import qualified Language.Parser.Ptera.Machine.LAPEG          as LAPEG


type T start varDoc altDoc a = BuilderT start varDoc altDoc a

type BuilderT start varDoc altDoc a = StateT (Context start varDoc altDoc a)

data Context start varDoc altDoc a = Context
    { ctxInitials    :: EnumMap.EnumMap start LAPEG.VarNum
    , ctxNextVarNum     :: LAPEG.VarNum
    , ctxNextAltNum :: LAPEG.AltNum
    , ctxVars        :: AlignableMap.T LAPEG.VarNum (PEG.Var varDoc)
    , ctxRules       :: AlignableMap.T LAPEG.VarNum LAPEG.Rule
    , ctxAlts :: AlignableMap.T LAPEG.AltNum (LAPEG.Alt altDoc a)
    }
    deriving (Eq, Show, Functor)

build :: Monad m
    => BuilderT start varDoc altDoc a m () -> m (LAPEG.T start varDoc altDoc a)
build builder = do
    finalCtx <- execStateT builder initialCtx
    pure do
        LAPEG.LAPEG
            { initials = ctxInitials finalCtx
            , rules = AlignableArray.fromTotalMap
                do ctxNextVarNum finalCtx
                do ctxRules finalCtx
            , vars = AlignableArray.fromTotalMap
                do ctxNextVarNum finalCtx
                do ctxVars finalCtx
            , alts = AlignableArray.fromTotalMap
                do ctxNextAltNum finalCtx
                do ctxAlts finalCtx
            }
    where
        initialCtx = Context
            { ctxInitials = EnumMap.empty
            , ctxNextVarNum = Alignable.initialAlign
            , ctxNextAltNum = Alignable.initialAlign
            , ctxRules = AlignableMap.empty
            , ctxVars = AlignableMap.empty
            , ctxAlts = AlignableMap.empty
            }

genNewVar :: Monad m
    => PEG.Var varDoc -> BuilderT start varDoc altDoc a m LAPEG.VarNum
genNewVar v = do
    vn <- ctxNextVarNum <$> get
    modify' \ctx -> ctx
        { ctxNextVarNum = Alignable.nextAlign vn
        , ctxVars = AlignableMap.insert vn v
            do ctxVars ctx
        }
    pure vn

genNewAlt :: Monad m
    => LAPEG.Alt altDoc a -> BuilderT start varDoc altDoc a m LAPEG.AltNum
genNewAlt alt = do
    altn <- ctxNextAltNum <$> get
    modify' \ctx -> ctx
        { ctxNextAltNum = Alignable.nextAlign altn
        , ctxAlts = AlignableMap.insert altn alt
            do ctxAlts ctx
        }
    pure altn

addInitial :: Monad m => Enum start
    => start -> LAPEG.VarNum -> BuilderT start varDoc altDoc a m ()
addInitial i v = modify' \ctx -> ctx
    { ctxInitials = EnumMap.insert i v do ctxInitials ctx
    }

addRule :: Monad m
    => LAPEG.VarNum -> LAPEG.Rule -> BuilderT start varDoc altDoc a m ()
addRule v e = modify' \ctx -> ctx
    { ctxRules = AlignableMap.insert v e
        do ctxRules ctx
    }
