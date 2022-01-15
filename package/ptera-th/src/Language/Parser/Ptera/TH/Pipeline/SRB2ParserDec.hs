{-# LANGUAGE TemplateHaskell #-}

module Language.Parser.Ptera.TH.Pipeline.SRB2ParserDec where

import           Language.Parser.Ptera.Prelude

import qualified Data.Bits                                  as Bits
import qualified Data.EnumMap.Strict                        as EnumMap
import qualified Data.HashMap.Strict                        as HashMap
import qualified Language.Haskell.TH                        as TH
import qualified Language.Haskell.TH.Syntax                 as TH
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Language.Parser.Ptera.Data.Symbolic.IntMap as SymbolicIntMap
import qualified Language.Parser.Ptera.Machine.PEG        as PEG
import qualified Language.Parser.Ptera.Machine.LAPEG        as LAPEG
import qualified Language.Parser.Ptera.Machine.SRB          as SRB
import qualified Language.Parser.Ptera.Syntax.Grammar       as Grammar
import qualified Language.Parser.Ptera.TH.Data.Bits.MaxBit  as Bits
import           Language.Parser.Ptera.TH.ParserLib
import qualified Language.Parser.Ptera.TH.Syntax            as Syntax

type SemanticAction ctx = Grammar.Action (Syntax.SemActM ctx)

data PipelineParam = PipelineParam
    {
        startsTy    :: TH.Q TH.Type,
        rulesTy     :: TH.Q TH.Type,
        tokensTy    :: TH.Q TH.Type,
        tokenTy     :: TH.Q TH.Type,
        customCtxTy :: TH.Q TH.Type,
        tokenBounds :: (Int, Int)
    }

srb2QParser
    :: PipelineParam
    -> SRB.T Int StringLit (Maybe altDoc) (SemanticAction ctx)
    -> TH.Q [TH.Dec]
srb2QParser param srb = do
    let runnerFnName = TH.mkName "pteraTHRunner"
    let parserInitialFnName = TH.mkName "pteraTHParserInitial"
    let parserGetTokenNumFnName = TH.mkName "pteraTHParserGetTokenNum"
    let parserTransFnName = TH.mkName "pteraTHParserTrans"
    let parserAltKindFnName = TH.mkName "pteraTHParserAltKind"
    let parserStateHelpFnName = TH.mkName "pteraTHParserStateHelp"
    let parserAltHelpFnName = TH.mkName "pteraTHParserAltHelp"
    let parserActionFnName = TH.mkName "pteraTHParserAction"

    sequence
        [ TH.SigD parserInitialFnName <$>
            [t|Int -> Maybe Int|]
        , outputParserInitialFn parserInitialFnName do SRB.initials srb

        , TH.SigD parserGetTokenNumFnName <$>
            [t|$(tokenTy param) -> Int|]
        , TH.ValD do TH.VarP parserGetTokenNumFnName
            <$> fmap TH.NormalB [e|\t ->
                pteraTHTokenToTerminal (Proxy :: Proxy $(tokensTy param)) t
            |]
            <*> pure []

        , TH.SigD parserTransFnName <$>
            [t|Int -> Int -> Trans|]
        , outputParserTransFn parserTransFnName
            do tokenBounds param
            do SRB.states srb

        , TH.SigD parserAltKindFnName <$>
            [t|Int -> AltKind|]
        , outputParserAltKindFn parserAltKindFnName do SRB.alts srb

        , TH.SigD parserStateHelpFnName <$>
            [t|Int -> [(Int, Int)]|]
        , outputParserStateHelpFn parserStateHelpFnName
            do SRB.states srb

        , TH.SigD parserAltHelpFnName <$>
            [t|Int -> (StringLit, Maybe ())|]
        , outputParserAltHelpFn parserAltHelpFnName
            do SRB.alts srb
            do SRB.vars srb

        , TH.SigD parserActionFnName <$>
            [t|Int -> ActionM $(customCtxTy param)|]
        , outputParserActionFn parserActionFnName do SRB.alts srb

        , TH.SigD runnerFnName <$>
            [t|Parser
                $(customCtxTy param)
                $(rulesTy param)
                $(tokenTy param)
                $(startsTy param)
            |]
        , outputRunnerFn runnerFnName
            parserInitialFnName
            parserGetTokenNumFnName
            parserTransFnName
            parserAltKindFnName
            parserStateHelpFnName
            parserAltHelpFnName
            parserActionFnName
        ]

outputParserInitialFn :: TH.Name -> EnumMap.EnumMap Int SRB.StateNum -> TH.Q TH.Dec
outputParserInitialFn parserInitialFnName initials =
        TH.ValD do TH.VarP parserInitialFnName
            <$> fmap TH.NormalB [e|\s ->
                if s <= $(TH.lift ub)
                    then pteraTHArrayIndex table s
                    else Nothing
            |]
            <*> [d|
                table = pteraTHArrayFromList $(TH.lift ub) $(TH.ListE <$> qes)
            |]
    where
        ub = if EnumMap.null initials
            then -1
            else let (i, _) = EnumMap.findMax initials in i

        qes = traverse
            do \i -> case EnumMap.lookup i initials of
                Nothing ->
                    [e|Nothing|]
                Just (SRB.StateNum s) ->
                    [e|Just $(TH.lift s)|]
            [0..ub]

outputParserTransFn :: TH.Name
    -> (Int, Int)
    -> AlignableArray.T SRB.StateNum SRB.MState
    -> TH.Q TH.Dec
outputParserTransFn parserTransFnName (minTokBound, maxTokBound) states = if
    | tokBitSize + stateBitSize > 29 ->
        error "exceed over bit size limited"
    | otherwise ->
        TH.ValD do TH.VarP parserTransFnName
            <$> fmap TH.NormalB [e|\s0 c0 ->
                let c1 = if c0 >= 0
                        then c0 - $(TH.lift minTokBound)
                        else $(TH.lift maxTokBound) + 1
                    s1 = $(stateTableLookupFn)
                        $(TH.lift tokBitSize)
                        stateTable
                        s0 c1
                    opsNum = $(opsNumTableLookupFn)
                        $(TH.lift tokBitSize)
                        opsNumTable
                        s0 c1
                    ops = pteraTHArrayIndex opsArr opsNum
                in Trans s1 ops
            |]
            <*> [d|
                stateTable = $(tableAddrExp stateByteSize
                    do reverse do outTransReprStates outTrans)
                opsNumTable = $(tableAddrExp opsNumByteSize
                    do reverse do outTransReprOpsNums outTrans)
                opsArr = pteraTHArrayFromList $(TH.lift opsNumMax)
                    $(TH.ListE <$>
                        do sequence do reverse do outTransReprTransOps outTrans)
            |]
    where
        tokBitSize = Bits.maxBitSize
            -- input tokens + special tokens (-1)
            do maxTokBound - minTokBound + 1
        tokMax = (1 `Bits.shiftL` tokBitSize) - 1
        stateBitSize = Bits.maxBitSize do length states - 1
        stateByteSize = if
            | stateBitSize <= 8  -> 1
            | stateBitSize <= 16 -> 2
            | otherwise          -> 4
        stateTableLookupFn = if
            | stateBitSize <= 8  -> [e|pteraTHLookupTable8|]
            | stateBitSize <= 16 -> [e|pteraTHLookupTable16|]
            | otherwise          -> [e|pteraTHLookupTable32|]

        outTrans = genOutTransRepr tokMax states
        opsNumMax = outTransReprNextOpsNum outTrans - 1
        opsNumBitSize = Bits.maxBitSize opsNumMax
        opsNumByteSize = if
            | opsNumBitSize <= 8  -> 1
            | opsNumBitSize <= 16 -> 2
            | otherwise           -> 4
        opsNumTableLookupFn = if
            | opsNumBitSize <= 8  -> [e|pteraTHLookupTable8|]
            | opsNumBitSize <= 16 -> [e|pteraTHLookupTable16|]
            | otherwise           -> [e|pteraTHLookupTable32|]

genOutTransRepr :: Int -> AlignableArray.T SRB.StateNum SRB.MState -> OutTransRepr
genOutTransRepr tokMax states = foldl'
    do \acc0 srbState -> do
        let srbTrans = SRB.stateTrans srbState
        foldl'
            do \acc1 i -> do
                let (toSn, opsRepr) = case SymbolicIntMap.lookup i srbTrans of
                        Just (SRB.TransWithOps ops (SRB.StateNum x)) -> do
                            (x, OutTransWithOpsRepr ops)
                        Just (SRB.TransReduce alt) ->
                            (-1, OutTransReduce alt)
                        Nothing ->
                            (-1, OutTransWithOpsRepr [])
                let opsMap0 = outTransReprOpsMap acc1
                let nextOpsNum0 = outTransReprNextOpsNum acc1
                let transOps0 = outTransReprTransOps acc1
                let (opsNum, opsMap1, nextOpsNum1, transOps1) =
                        case HashMap.lookup opsRepr opsMap0 of
                            Just x ->
                                ( x
                                , opsMap0
                                , nextOpsNum0
                                , transOps0
                                )
                            Nothing ->
                                ( nextOpsNum0
                                , HashMap.insert opsRepr nextOpsNum0 opsMap0
                                , nextOpsNum0 + 1
                                , toTransOpsExp opsRepr:transOps0
                                )
                OutTransRepr
                    { outTransReprStates = toSn:outTransReprStates acc1
                    , outTransReprOpsNums = opsNum:outTransReprOpsNums acc1
                    , outTransReprTransOps = transOps1
                    , outTransReprNextOpsNum = nextOpsNum1
                    , outTransReprOpsMap = opsMap1
                    }
            do acc0
            do [0..tokMax]
    do OutTransRepr
        {
            outTransReprStates = [],
            outTransReprOpsNums = [],
            outTransReprTransOps = [],
            outTransReprNextOpsNum = 0,
            outTransReprOpsMap = HashMap.empty
        }
    do toList states

tableAddrExp :: Int -> [Int] -> TH.Q TH.Exp
tableAddrExp unitSize ns = pure
    do TH.LitE
        do TH.StringPrimL
            do concatMap
                do \sn -> addrCodeUnitsLE unitSize
                    do fromEnum sn
                do ns

-- | Should correspond @pteraTHLookupTable*@
addrCodeUnitsLE :: Int -> Int -> [Word8]
addrCodeUnitsLE unitSize n
    | n >= 0    = take unitSize
        do map
            do \m -> fromInteger do toInteger do mod8bit m
            do iterate (`Bits.shiftR` 8) n
    | n == -1   = replicate unitSize 0xFF
    | otherwise = error "unsupported"
    where
        mod8bit = case Bits.bitSizeMaybe n of
            Nothing -> \x -> x Bits..&. 0xFF
            Just bs
                | bs <= 8   -> \x -> x
                | otherwise -> \x -> x Bits..&. 0xFF

toTransOpsExp :: OutTransOpsRepr -> TH.Q TH.Exp
toTransOpsExp = \case
    OutTransWithOpsRepr ops ->
        TH.ListE <$> traverse toTransOpExp ops
    OutTransReduce (LAPEG.AltNum alt) ->
        [e|[TransOpReduce $(TH.lift alt)]|]

toTransOpExp :: SRB.TransOp -> TH.Q TH.Exp
toTransOpExp = \case
    SRB.TransOpEnter (LAPEG.VarNum v) needBack msn -> do
        let sn = case msn of
                Nothing ->
                    -1
                Just (SRB.StateNum x) ->
                    x
        [e|TransOpEnter $(TH.lift v) $(TH.lift needBack) $(TH.lift sn)|]
    SRB.TransOpPushBackpoint (SRB.StateNum s) ->
        [e|TransOpPushBackpoint $(TH.lift s)|]
    SRB.TransOpHandleNot (LAPEG.AltNum alt) ->
        [e|TransOpHandleNot $(TH.lift alt)|]
    SRB.TransOpShift ->
        [e|TransOpShift|]

data OutTransRepr = OutTransRepr
    {
        outTransReprStates     :: [Int],
        outTransReprOpsNums    :: [Int],
        outTransReprTransOps   :: [TH.Q TH.Exp],
        outTransReprNextOpsNum :: Int,
        outTransReprOpsMap     :: HashMap.HashMap OutTransOpsRepr Int
    }

data OutTransOpsRepr
    = OutTransWithOpsRepr [SRB.TransOp]
    | OutTransReduce LAPEG.AltNum
    deriving (Eq, Show, Generic)

instance Hashable OutTransOpsRepr

outputParserAltKindFn
    :: TH.Name -> AlignableArray.T LAPEG.AltNum (LAPEG.Alt altDoc a)
    -> TH.Q TH.Dec
outputParserAltKindFn parserAltKindFnName alts = TH.ValD
    do TH.VarP parserAltKindFnName
    <$> fmap TH.NormalB [e|
        let arr = pteraTHArrayFromList $(TH.lift do length alts - 1)
                $(TH.ListE <$> traverse altKindExp do toList alts)
        in \i -> pteraTHArrayIndex arr i
    |]
    <*> pure []
    where
        altKindExp alt = case LAPEG.altKind alt of
            AltSeq -> [e|AltSeq|]
            AltAnd -> [e|AltAnd|]
            AltNot -> [e|AltNot|]

outputParserStateHelpFn
    :: TH.Name
    -> AlignableArray.T SRB.StateNum SRB.MState
    -> TH.Q TH.Dec
outputParserStateHelpFn fnName states = TH.ValD
    do TH.VarP fnName
    <$> fmap TH.NormalB [e|
        let arr = pteraTHArrayFromList $(TH.lift do length states - 1)
                $(TH.ListE <$> traverse stateHelpExp do toList states)
        in \i -> pteraTHArrayIndex arr i
    |]
    <*> pure []
    where
        stateHelpExp st =
            let altItems = SRB.stateAltItems st
            in TH.ListE <$> forM altItems \altItem -> do
                let altNum :: Int = coerce do SRB.altItemAltNum altItem
                    pos :: Int = coerce do SRB.altItemCurPos altItem
                [e|($(TH.lift altNum), $(TH.lift pos))|]

outputParserAltHelpFn
    :: TH.Name
    -> AlignableArray.T LAPEG.AltNum (LAPEG.Alt altDoc a)
    -> AlignableArray.T LAPEG.VarNum (PEG.Var StringLit)
    -> TH.Q TH.Dec
outputParserAltHelpFn parserAltHelpFnName alts vars = TH.ValD
    do TH.VarP parserAltHelpFnName
    <$> fmap TH.NormalB [e|
        let arr = pteraTHArrayFromList $(TH.lift do length alts - 1)
                $(TH.ListE <$> traverse altHelpExp do toList alts)
        in \i -> pteraTHArrayIndex arr i
    |]
    <*> pure []
    where
        altHelpExp alt =
            let v = AlignableArray.forceIndex vars do LAPEG.altVar alt
            in [e|($(TH.lift do PEG.varHelp v), Nothing)|]

outputParserActionFn
    :: TH.Name
    -> AlignableArray.T LAPEG.AltNum (LAPEG.Alt altHelp (SemanticAction ctx))
    -> TH.Q TH.Dec
outputParserActionFn parserActionFnName alts = TH.ValD
    do TH.VarP parserActionFnName
    <$> fmap TH.NormalB [e|
        let arr = pteraTHArrayFromList $(TH.lift do length alts - 1)
                $(pure do
                    TH.ListE
                        [ TH.VarE do altActionForAltFnName n
                        | (n, _) <- AlignableArray.assocs alts
                        ]
                )
        in \i -> pteraTHArrayIndex arr i
    |]
    <*> traverse
        do \(n, alt) -> TH.ValD
            do TH.VarP do altActionForAltFnName n
            <$> do TH.NormalB <$> altActionExp alt
            <*> pure []
        do AlignableArray.assocs alts
    where
        altActionForAltFnName (LAPEG.AltNum n) = TH.mkName
            do "pteraTHParserActionForAlt" ++ show n

        altActionExp alt = case LAPEG.altAction alt of
            Grammar.Action act -> [e|
                pteraTHAction $(Syntax.unsafeSemanticAction act)
                |]

outputRunnerFn
    :: TH.Name -> TH.Name -> TH.Name -> TH.Name -> TH.Name -> TH.Name -> TH.Name -> TH.Name
    -> TH.Q TH.Dec
outputRunnerFn runnerFnName = \
        parserInitialFnName
        parserGetTokenNumFnName
        parserTransFnName
        parserAltKindFnName
        parserStateHelpFnName
        parserAltHelpFnName
        parserActionFnName
    -> TH.ValD do TH.VarP runnerFnName
        <$> fmap TH.NormalB [e|pteraTHUnsafeRunner parser|]
        <*> [d|
            parser = RunnerParser
                $(pure do TH.VarE parserInitialFnName)
                $(pure do TH.VarE parserGetTokenNumFnName)
                $(pure do TH.VarE parserTransFnName)
                $(pure do TH.VarE parserAltKindFnName)
                $(pure do TH.VarE parserStateHelpFnName)
                $(pure do TH.VarE parserAltHelpFnName)
                $(pure do TH.VarE parserActionFnName)
        |]
