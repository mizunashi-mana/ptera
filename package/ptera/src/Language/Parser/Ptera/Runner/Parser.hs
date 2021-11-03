module Language.Parser.Ptera.Runner.Parser where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Data.HList     as HList
import qualified Language.Parser.Ptera.Machine.PEG    as PEG
import qualified Language.Parser.Ptera.Syntax         as Syntax
import qualified Language.Parser.Ptera.Syntax.Grammar as Grammar
import qualified Unsafe.Coerce                        as Unsafe

type StateNum = Int
type TokenNum = Int
type VarNum = Int
type AltNum = Int

type T = Parser

type Action = Grammar.Action Syntax.SemanticAction

runAction :: Action -> HList.T us -> a
runAction (Grammar.Action (Syntax.SemanticAction act)) = coerceAct act where
    coerceAct :: (HList.T us1 -> a1) -> HList.T us2 -> a2
    coerceAct = Unsafe.unsafeCoerce

data Parser s e = Parser
    {
        parserInitial     :: s -> Maybe StateNum,
        parserGetTokenNum :: e -> TokenNum,
        parserTrans       :: StateNum -> TokenNum -> Maybe Trans,
        parserAltKind     :: AltNum -> PEG.AltKind,
        parserActions     :: AltNum -> Action
    }

data Trans = Trans
    {
        transState :: StateNum,
        transOps   :: [TransOp]
    }
    deriving (Eq, Show)

data TransOp
    = TransOpEnter VarNum StateNum
    | TransOpPushBackpoint StateNum
    | TransOpHandleNot AltNum
    | TransOpShift
    | TransOpReduce AltNum
    deriving (Eq, Show)

eosToken :: TokenNum
eosToken = -1
