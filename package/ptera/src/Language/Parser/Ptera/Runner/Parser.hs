module Language.Parser.Ptera.Runner.Parser (
    T,

    StartNum,
    StateNum,
    TokenNum,
    VarNum,
    AltNum,
    AltKind (..),

    RunnerParser (..),
    Syntax.GrammarToken (..),
    ActionM (..),
    ReduceArgument (..),
    Syntax.ActionTask (..),
    Syntax.getAction,
    Syntax.modifyAction,
    Syntax.failAction,
    Trans (..),
    TransOp (..),

    eosToken,
) where

import           Language.Parser.Ptera.Prelude

import           Language.Parser.Ptera.Machine.PEG (AltKind (..))
import qualified Language.Parser.Ptera.Syntax      as Syntax

type StartNum = Int
type StateNum = Int
type TokenNum = Int
type VarNum = Int
type AltNum = Int

type T = RunnerParser

newtype ActionM ctx = ActionM
    { runActionM :: [ReduceArgument] -> Syntax.ActionTask ctx ReduceArgument
    }

data ReduceArgument where
    ReduceArgument :: a -> ReduceArgument

data RunnerParser ctx elem altHelp = RunnerParser
    { parserInitial     :: StartNum -> Maybe StateNum
    , parserGetTokenNum :: elem -> TokenNum
    , parserTrans       :: StateNum -> TokenNum -> Trans
    , parserAltKind     :: AltNum -> AltKind
    , parserStateHelp   :: StateNum -> [(AltNum, Int)]
    , parserAltHelp     :: AltNum -> (StringLit, Maybe altHelp)
    , parserAction      :: AltNum -> ActionM ctx
    }

data Trans = Trans
    { transState :: StateNum
    , transOps   :: [TransOp]
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
