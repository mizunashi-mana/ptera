module Language.Parser.Ptera.Machine.LAPEG.RuleBuilder where

import           Language.Parser.Ptera.Prelude

import qualified Data.List.NonEmpty                         as NonEmpty
import qualified Language.Parser.Ptera.Data.Symbolic.IntMap as SymbolicIntMap
import qualified Language.Parser.Ptera.Data.Symbolic.IntSet as SymbolicIntSet
import qualified Language.Parser.Ptera.Machine.LAPEG        as LAPEG


type T a = BuilderT a

type BuilderT = StateT Context

newtype Context = Context
    {
        ctxAlts :: SymbolicIntMap.T (NonEmpty LAPEG.AltNum)
    }
    deriving (Eq, Show)

build :: Monad m => BuilderT m () -> m LAPEG.Rule
build builder = do
    finalCtx <- execStateT builder initialCtx
    pure do
        LAPEG.Rule do
            SymbolicIntMap.groupBy
                do \alts -> NonEmpty.reverse alts
                do ctxAlts finalCtx
    where
        initialCtx = Context
            {
               ctxAlts = SymbolicIntMap.empty
            }

addAlt :: Monad m => SymbolicIntSet.T -> LAPEG.AltNum -> BuilderT m ()
addAlt is alt = modify' \ctx -> ctx
    {
        ctxAlts = SymbolicIntMap.alterBulk
            do \case
                Nothing   -> Just do pure alt
                Just alts -> Just do alt NonEmpty.<| alts
            do is
            do ctxAlts ctx
    }
