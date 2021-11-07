module Language.Parser.Ptera.Pipeline.SRB2Parser where

import Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Machine.SRB as SRB
import qualified Language.Parser.Ptera.Machine.LAPEG as LAPEG
import qualified Language.Parser.Ptera.Runner.Parser as Parser
import qualified Data.EnumMap.Strict as EnumMap
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Symbolic.IntMap as SymbolicIntMap

srb2Parser :: GenParam e -> SRB.T Int Parser.Action -> Parser.T e
srb2Parser genParam srb = Parser.Parser
    { parserInitial = \s -> coerce do EnumMap.lookup s do SRB.initials srb
    , parserGetTokenNum = genParamGetToken genParam
    , parserTrans = \s0 t -> if s0 < 0
        then Nothing
        else
            let srbSt = AlignableArray.forceIndex
                    do SRB.states srb
                    do SRB.StateNum s0
            in goTrans t srbSt
    , parserAltKind = \alt -> LAPEG.altKind
        do AlignableArray.forceIndex
            do SRB.alts srb
            do alt
    , parserActions = \alt -> LAPEG.altAction
        do AlignableArray.forceIndex
            do SRB.alts srb
            do alt
    }
    where
        goTrans t srbSt = case SymbolicIntMap.lookup t do SRB.stateTrans srbSt of
            Nothing ->
                Nothing
            Just (SRB.TransWithOps ops (SRB.StateNum s1)) ->
                Just do
                    Parser.Trans
                        {
                            transState = s1,
                            transOps = goTransOp <$> ops
                        }
            Just (SRB.TransReduce alt) ->
                Just do
                    Parser.Trans
                        {
                            transState = -1,
                            transOps = [Parser.TransOpReduce alt]
                        }

        goTransOp = \case
            SRB.TransOpEnter v mEnterSn ->
                let enterSn = case mEnterSn of
                        Nothing ->
                            -1
                        Just (SRB.StateNum x) ->
                            x
                in Parser.TransOpEnter v enterSn
            SRB.TransOpPushBackpoint (SRB.StateNum backSn) ->
                Parser.TransOpPushBackpoint backSn
            SRB.TransOpHandleNot alt ->
                Parser.TransOpHandleNot alt
            SRB.TransOpShift ->
                Parser.TransOpShift

data GenParam e = GenParam
    {
        genParamGetToken :: e -> Parser.TokenNum
    }
