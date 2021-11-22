module Language.Parser.Ptera.Pipeline.SRB2Parser where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Symbolic.IntMap as SymbolicIntMap
import qualified Language.Parser.Ptera.Machine.LAPEG        as LAPEG
import qualified Language.Parser.Ptera.Machine.SRB          as SRB
import qualified Language.Parser.Ptera.Runner.Parser        as Parser
import qualified Language.Parser.Ptera.Syntax as Syntax
import qualified Language.Parser.Ptera.Syntax.Grammar       as Grammar
import qualified Unsafe.Coerce as Unsafe
import qualified Language.Parser.Ptera.Data.HList as HList

type Action = Grammar.Action Syntax.SemAct

srb2Parser :: forall e. Syntax.GrammarToken e => SRB.T Int Action -> Parser.T e
srb2Parser srb = Parser.Parser
    { parserInitial = \s -> coerce do EnumMap.lookup s do SRB.initials srb
    , parserGetTokenNum = \tok -> fromEnum do Syntax.tokenToTerminal @e tok
    , parserTrans = \s0 t -> if s0 < 0
        then Nothing
        else
            let srbSt = AlignableArray.forceIndex
                    do SRB.states srb
                    do SRB.StateNum s0
            in buildTrans t srbSt
    , parserAltKind = \alt -> LAPEG.altKind
        do AlignableArray.forceIndex
            do SRB.alts srb
            do alt
    , parserAction = \alt -> runAction
        do LAPEG.altAction
            do AlignableArray.forceIndex
                do SRB.alts srb
                do alt
    }

buildTrans :: Int -> SRB.MState -> Maybe Parser.Trans
buildTrans t srbSt = case SymbolicIntMap.lookup t do SRB.stateTrans srbSt of
    Nothing ->
        Nothing
    Just (SRB.TransWithOps ops (SRB.StateNum s1)) ->
        Just do
            Parser.Trans
                {
                    transState = s1,
                    transOps = transOp <$> ops
                }
    Just (SRB.TransReduce alt) ->
        Just do
            Parser.Trans
                {
                    transState = -1,
                    transOps = [Parser.TransOpReduce alt]
                }

transOp :: SRB.TransOp -> Parser.TransOp
transOp = \case
    SRB.TransOpEnter needBack v mEnterSn ->
        let enterSn = case mEnterSn of
                Nothing ->
                    -1
                Just (SRB.StateNum x) ->
                    x
        in Parser.TransOpEnter needBack v enterSn
    SRB.TransOpPushBackpoint (SRB.StateNum backSn) ->
        Parser.TransOpPushBackpoint backSn
    SRB.TransOpHandleNot alt ->
        Parser.TransOpHandleNot alt
    SRB.TransOpShift ->
        Parser.TransOpShift

runAction :: Grammar.Action Syntax.SemAct -> Parser.Action
runAction (Grammar.Action (Syntax.SemAct f)) = Parser.Action \l ->
        Unsafe.unsafeCoerce do f do goL l
    where
        goL = \case
            [] -> Unsafe.unsafeCoerce HList.HNil
            x:xs -> Unsafe.unsafeCoerce do x HList.:* goL xs