module Language.Parser.Ptera.Pipeline.SRB2Parser where

import           Language.Parser.Ptera.Prelude

import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.HEnum           as HEnum
import qualified Language.Parser.Ptera.Data.HList           as HList
import qualified Language.Parser.Ptera.Data.Symbolic.IntMap as SymbolicIntMap
import qualified Language.Parser.Ptera.Machine.LAPEG        as LAPEG
import qualified Language.Parser.Ptera.Machine.SRB          as SRB
import qualified Language.Parser.Ptera.Runner.Parser        as Parser
import qualified Language.Parser.Ptera.Syntax               as Syntax
import qualified Language.Parser.Ptera.Syntax.Grammar       as Grammar
import qualified Unsafe.Coerce                              as Unsafe

type Action ctx = Grammar.Action (Syntax.SemActM ctx)

srb2Parser :: forall ctx tokens elem. Syntax.GrammarToken tokens elem
    => Proxy tokens -> SRB.T Int (Action ctx) -> Parser.T ctx elem
srb2Parser p srb = Parser.RunnerParser
    { parserInitial = \s -> coerce do EnumMap.lookup s do SRB.initials srb
    , parserGetTokenNum = \tok ->
        HEnum.unsafeHEnum do Syntax.tokenToTerminal p tok
    , parserTrans = \s0 t -> if s0 < 0
        then Parser.Trans
            {
                transState = -1,
                transOps = []
            }
        else
            let srbSt = AlignableArray.forceIndex
                    do SRB.states srb
                    do SRB.StateNum s0
            in buildTrans t srbSt
    , parserAltKind = \alt -> LAPEG.altKind
        do AlignableArray.forceIndex
            do SRB.alts srb
            do LAPEG.AltNum alt
    , parserAction = \alt -> runAction
        do LAPEG.altAction
            do AlignableArray.forceIndex
                do SRB.alts srb
                do LAPEG.AltNum alt
    }

buildTrans :: Int -> SRB.MState -> Parser.Trans
buildTrans t srbSt = case SymbolicIntMap.lookup t do SRB.stateTrans srbSt of
    Nothing ->
        Parser.Trans
            {
                transState = -1,
                transOps = []
            }
    Just (SRB.TransWithOps ops (SRB.StateNum s1)) ->
        Parser.Trans
            {
                transState = s1,
                transOps = transOp <$> ops
            }
    Just (SRB.TransReduce (LAPEG.AltNum alt)) ->
        Parser.Trans
            {
                transState = -1,
                transOps = [Parser.TransOpReduce alt]
            }

transOp :: SRB.TransOp -> Parser.TransOp
transOp = \case
    SRB.TransOpEnter (LAPEG.Var v) needBack mEnterSn ->
        let enterSn = case mEnterSn of
                Nothing ->
                    -1
                Just (SRB.StateNum x) ->
                    x
        in Parser.TransOpEnter v needBack enterSn
    SRB.TransOpPushBackpoint (SRB.StateNum backSn) ->
        Parser.TransOpPushBackpoint backSn
    SRB.TransOpHandleNot (LAPEG.AltNum alt) ->
        Parser.TransOpHandleNot alt
    SRB.TransOpShift ->
        Parser.TransOpShift

runAction :: Action ctx -> Parser.ActionM ctx
runAction (Grammar.Action (Syntax.SemActM f)) = Parser.ActionM \l ->
        unsafeCoerceActionTask do f do goL l
    where
        goL = \case
            []   -> unsafeCoerceHList HList.HNil
            x:xs -> unsafeCoerceHList do x HList.:* goL xs

        unsafeCoerceHList :: HList.T us1 -> HList.T us2
        unsafeCoerceHList = Unsafe.unsafeCoerce

        unsafeCoerceActionTask :: Syntax.ActionTask ctx a -> Syntax.ActionTask ctx b
        unsafeCoerceActionTask = Unsafe.unsafeCoerce
