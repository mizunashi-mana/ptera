module Language.Parser.Ptera.Runner.Parser (
    T,

    StartNum,
    StateNum,
    TokenNum,
    VarNum,
    AltNum,
    AltKind (..),

    Parser (..),
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

type T = Parser

newtype Action = Action
    {
        runAction :: forall a b. [a] -> b
    }

data Parser e = Parser
    {
        parserInitial     :: StartNum -> Maybe StateNum,
        parserGetTokenNum :: e -> TokenNum,
        parserTrans       :: StateNum -> TokenNum -> Trans,
        parserAltKind     :: AltNum -> AltKind,
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
