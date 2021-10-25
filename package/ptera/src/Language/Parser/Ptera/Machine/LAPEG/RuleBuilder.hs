module Language.Parser.Ptera.Machine.LAPEG.RuleBuilder where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Data.Symbolic.IntSet as SymbolicIntSet
import qualified Language.Parser.Ptera.Machine.LAPEG        as LAPEG


type T a = BuilderT a

type BuilderT = StateT Context

data Context = Context
    {
        ctxRange :: SymbolicIntSet.T,
        ctxAlts :: [LAPEG.AltNum]
    }
    deriving (Eq, Show)

build :: Monad m => BuilderT m () -> m LAPEG.Rule
build builder = do
    finalCtx <- execStateT builder initialCtx
    pure do
        LAPEG.Rule
            {
                ruleRange = ctxRange finalCtx,
                ruleAlts = reverse do ctxAlts finalCtx
            }
    where
        initialCtx = Context
            {
                ctxRange = mempty,
                ctxAlts = []
            }

addAlt :: Monad m => SymbolicIntSet.T -> LAPEG.AltNum -> BuilderT m ()
addAlt is alt = modify' \ctx -> ctx
    { ctxRange = SymbolicIntSet.union is do ctxRange ctx
    , ctxAlts = alt:ctxAlts ctx
    }
