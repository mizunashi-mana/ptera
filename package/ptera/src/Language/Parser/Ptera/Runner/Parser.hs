module Language.Parser.Ptera.Runner.Parser (
    T,

    StartNum,
    StateNum,
    TokenNum,
    VarNum,
    AltNum,
    AltKind (..),

    RunnerParser (..),
    Action (..),
    Trans (..),
    TransOp (..),

    eosToken,
) where

import           Language.Parser.Ptera.Prelude

import           Language.Parser.Ptera.Machine.PEG (AltKind (..))

type StartNum = Int
type StateNum = Int
type TokenNum = Int
type VarNum = Int
type AltNum = Int

type T = RunnerParser

newtype Action feed = Action
    {
        runAction :: forall a b. [a] -> ActionResult feed b
    }

data ActionResult feed a = ActionResult
    {
        actionFeed :: Maybe feed,
        actionResult :: a
    }

data RunnerParser feed elem = RunnerParser
    {
        parserInitial     :: StartNum -> Maybe StateNum,
        parserGetTokenNum :: elem -> TokenNum,
        parserTrans       :: StateNum -> TokenNum -> Trans,
        parserAltKind     :: AltNum -> AltKind,
        parserAction      :: AltNum -> Action feed
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
