module Language.Parser.Ptera.Machine.LAPEG.Builder where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable       as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Alignable.Map   as AlignableMap
import qualified Language.Parser.Ptera.Machine.LAPEG        as LAPEG


type T start doc a = BuilderT start doc a

type BuilderT start doc a = StateT (Context start doc a)

data Context start doc a = Context
    { ctxInitials   :: EnumMap.EnumMap start LAPEG.Var
    , ctxNextAltNum :: LAPEG.AltNum
    , ctxAlts       :: AlignableMap.T LAPEG.AltNum (LAPEG.Alt a)
    , ctxNextVar    :: LAPEG.Var
    , ctxRules      :: AlignableMap.T LAPEG.Var LAPEG.Rule
    , ctxDisplayVars :: AlignableMap.T LAPEG.Var doc
    }
    deriving (Eq, Show)

build :: Monad m => BuilderT start doc a m () -> m (LAPEG.T start doc a)
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
            , displayVars = AlignableArray.fromTotalMap
                do ctxNextVar finalCtx
                do ctxDisplayVars finalCtx
            }
    where
        initialCtx = Context
            { ctxInitials = EnumMap.empty
            , ctxNextAltNum = Alignable.initialAlign
            , ctxAlts = AlignableMap.empty
            , ctxNextVar = Alignable.initialAlign
            , ctxRules = AlignableMap.empty
            , ctxDisplayVars = AlignableMap.empty
            }

genNewAltNum :: Monad m => BuilderT start doc a m LAPEG.AltNum
genNewAltNum = do
    ctx <- get
    let n = ctxNextAltNum ctx
    put do ctx { ctxNextAltNum = Alignable.nextAlign n }
    pure n

genNewVar :: Monad m => doc -> BuilderT start doc a m LAPEG.Var
genNewVar d = do
    v <- ctxNextVar <$> get
    modify' \ctx -> ctx
        { ctxNextVar = Alignable.nextAlign v
        , ctxDisplayVars = AlignableMap.insert v d
            do ctxDisplayVars ctx
        }
    pure v

registerInitial :: Monad m => Enum start
    => start -> LAPEG.Var -> BuilderT start doc a m ()
registerInitial i v = modify' \ctx -> ctx
    {
        ctxInitials = EnumMap.insert i v do ctxInitials ctx
    }

addAlt :: Monad m => LAPEG.Alt a -> BuilderT start doc a m LAPEG.AltNum
addAlt alt = do
    n <- genNewAltNum
    modify' \ctx -> ctx
        {
            ctxAlts = AlignableMap.insert n alt
                do ctxAlts ctx
        }
    pure n

addRule :: Monad m => LAPEG.Var -> LAPEG.Rule -> BuilderT start doc a m ()
addRule v e = modify' \ctx -> ctx
    { ctxRules = AlignableMap.insert v e
        do ctxRules ctx
    }
