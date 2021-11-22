module Language.Parser.Ptera.Runner.Parser where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Machine.LAPEG as LAPEG
import qualified Language.Parser.Ptera.Machine.PEG   as PEG

type StartNum = Int
type StateNum = Int
type TokenNum = Int
type VarNum = LAPEG.Var
type AltNum = LAPEG.AltNum

type T = Parser

newtype Action = Action
    {
        runAction :: forall a b. [a] -> b
    }

data Parser e = Parser
    {
        parserInitial     :: StartNum -> Maybe StateNum,
        parserGetTokenNum :: e -> TokenNum,
        parserTrans       :: StateNum -> TokenNum -> Maybe Trans,
        parserAltKind     :: AltNum -> PEG.AltKind,
        parserAction      :: AltNum -> Action
    }

data Trans = Trans
    {
        transState :: StateNum,
        transOps   :: [TransOp]
    }
    deriving (Eq, Show)

data TransOp
    = TransOpEnter VarNum Bool StateNum
    | TransOpPushBackpoint StateNum
    | TransOpHandleNot AltNum
    | TransOpShift
    | TransOpReduce AltNum
    deriving (Eq, Show)

eosToken :: TokenNum
eosToken = -1
