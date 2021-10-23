module Language.Parser.Ptera.Machine.LAPEG.LAPEBuilder where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Machine.LAPEG        as LAPEG
import qualified Language.Parser.Ptera.Data.Symbolic.IntMap as SymbolicIntMap
import qualified Language.Parser.Ptera.Data.Symbolic.IntSet as SymbolicIntSet
import qualified Data.List.NonEmpty as NonEmpty


type T a = BuilderT a

type BuilderT a = StateT (Context a)

newtype Context a = Context
    {
        ctxAlts :: SymbolicIntMap.T (NonEmpty (LAPEG.Alt a))
    }
    deriving (Eq, Show)

build :: Monad m => BuilderT a m () -> m (LAPEG.LAPE a)
build builder = do
    finalCtx <- finalCtxM
    pure do
        LAPEG.LAPE
            { available = SymbolicIntMap.keys do ctxAlts finalCtx
            , alts = fmap
                do \alts -> NonEmpty.reverse alts
                do ctxAlts finalCtx
            }
    where
        initialCtx = Context
            {
               ctxAlts = SymbolicIntMap.empty
            }

        finalCtxM = execStateT builder initialCtx

addAlt :: Monad m => SymbolicIntSet.T -> LAPEG.Alt a -> BuilderT a m ()
addAlt is alt = modify' \ctx -> ctx
    {
        ctxAlts = SymbolicIntMap.alterBulk
            do \case
                Nothing   -> Just do pure alt
                Just alts -> Just do alt NonEmpty.<| alts
            do is
            do ctxAlts ctx
    }
