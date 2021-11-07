module Language.Parser.Ptera.Runner.RunT where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Data.Alignable     as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Map as AlignableMap
import qualified Language.Parser.Ptera.Data.HList         as HList
import qualified Language.Parser.Ptera.Machine.PEG        as PEG
import qualified Language.Parser.Ptera.Runner.Parser      as Parser
import qualified Language.Parser.Ptera.Scanner            as Scanner
import qualified Unsafe.Coerce                            as Unsafe


type T p e = RunT p e

type RunT p e = StateT (Context p e)

runT :: forall p e m a. Scanner.T p e m => RunT p e m (Result a)
runT = go where
    go = consumeIfNeeded >>= \case
        Nothing ->
            goResult
        Just (tok, _) -> transByInput tok >>= \case
            ContParse ->
                go
            CantContParse ->
                pure ParseFail

    goEos = transByInput Parser.eosToken >>= \case
        ContParse ->
            goResult
        CantContParse ->
            pure ParseFail

    goResult :: RunT p e m (Result a)
    goResult = get >>= \ctx -> case ctxItemStack ctx of
        [ItemArgument x] ->
            pure do Parsed do Unsafe.unsafeCoerce x
        _   ->
            goEos

data Result a
    = Parsed a
    | ParseFail
    deriving (Eq, Show, Functor)

data Context p e = Context
    {
        ctxParser         :: Parser.T e,
        ctxState          :: Parser.StateNum,
        ctxItemStack      :: [Item p],
        ctxLookAHeadToken :: Maybe (Parser.TokenNum, e),
        ctxCurrentPosition :: Position,
        ctxMemoTable      :: AlignableMap.T Position (AlignableMap.T Parser.VarNum (MemoItem p))
    }

newtype Position = Position Int
    deriving (Eq, Show)
    deriving Alignable.T via Alignable.Inst

data MemoItem p where
    MemoItem :: Position -> p -> a -> MemoItem p

data Item p where
    ItemEnter :: Position -> p -> Parser.VarNum -> Parser.StateNum -> Item p
    ItemHandleNot :: Parser.AltNum -> Item p
    ItemBackpoint :: p -> Parser.StateNum -> Item p
    ItemArgument :: a -> Item p

data RunningResult
    = ContParse
    | CantContParse
    deriving (Eq, Show)

initialContext :: Parser.T e -> Parser.StartNum -> Maybe (Context p e)
initialContext p s = do
    sn0 <- Parser.parserInitial p s
    pure do
        Context
            {
                ctxParser = p,
                ctxState = sn0,
                ctxLookAHeadToken = Nothing,
                ctxItemStack = [],
                ctxCurrentPosition = Alignable.initialAlign,
                ctxMemoTable = AlignableMap.empty
            }

transByInput :: forall p e m. Scanner.T p e m => Parser.TokenNum -> RunT p e m RunningResult
transByInput tok = go where
    go = do
        ctx0 <- get
        let mtrans1 = Parser.parserTrans
                do ctxParser ctx0
                do ctxState ctx0
                do tok
        case mtrans1 of
            Nothing ->
                parseFail
            Just trans1 -> do
                put do ctx0 { ctxState = Parser.transState trans1 }
                goTransOps do Parser.transOps trans1

    goTransOps :: [Parser.TransOp] -> RunT p e m RunningResult
    goTransOps = \case
        [] ->
            pure ContParse
        op:ops -> do
            result <- runTransOp op
            case result of
                ContParse ->
                    goTransOps ops
                CantContParse ->
                    pure CantContParse

runTransOp :: Scanner.T p e m => Parser.TransOp ->　RunT p e m RunningResult
runTransOp = \case
    Parser.TransOpEnter v enterSn ->
        runEnter v enterSn
    Parser.TransOpPushBackpoint backSn -> do
        mark <- lift Scanner.getMark
        pushItem do ItemBackpoint mark backSn
        pure ContParse
    Parser.TransOpHandleNot alt -> do
        pushItem do ItemHandleNot alt
        pure ContParse
    Parser.TransOpShift -> consumeIfNeeded >>= \case
        Nothing ->
            parseFail
        Just (_, x) -> do
            pushItem do ItemArgument x
            modify' \ctx -> ctx
                {
                    ctxLookAHeadToken = Nothing
                }
            pure ContParse
    Parser.TransOpReduce alt ->
        runReduce alt

runEnter :: Scanner.T p e m => Parser.VarNum -> Parser.StateNum -> RunT p e m RunningResult
runEnter v enterSn = do
    ctx <- get
    let pos0 = ctxCurrentPosition ctx
    let vm = case AlignableMap.lookup pos0 do ctxMemoTable ctx of
            Nothing -> AlignableMap.empty
            Just m  -> m
    case AlignableMap.lookup v vm of
        Just (MemoItem pos1 mark1 x) -> do
            put do
                ctx
                    {
                        ctxCurrentPosition = pos1,
                        ctxState = enterSn
                    }
            pushItem do ItemArgument x
            seekToMark mark1
            pure ContParse
        Nothing -> do
            pos1 <- ctxCurrentPosition <$> get
            mark1 <- lift Scanner.getMark
            pushItem do ItemEnter pos1 mark1 v enterSn
            pure ContParse

runReduce :: forall p e m. Scanner.T p e m => Parser.AltNum -> RunT p e m RunningResult
runReduce alt = do
        stack0 <- ctxItemStack <$> get
        go HList.HNil stack0
    where
        go :: HList.T us -> [Item p] -> RunT p e m RunningResult
        go args = \case
            [] ->
                pure CantContParse
            item:rest -> case item of
                ItemArgument x ->
                    go
                        do x HList.:* args
                        do rest
                ItemBackpoint{} ->
                    go args rest
                ItemHandleNot{} -> do
                    modify' \ctx -> ctx
                        {
                            ctxItemStack = rest
                        }
                    parseFail
                ItemEnter pos mark v enterSn -> do
                    modify' \ctx -> ctx
                        {
                            ctxItemStack = rest
                        }
                    goEnter args pos mark v enterSn

        goEnter :: HList.T us -> Position -> p -> Parser.VarNum -> Parser.StateNum
            -> RunT p e m RunningResult
        goEnter args pos0 mark0 v enterSn = do
            parser <- ctxParser <$> get
            case Parser.parserAltKind parser alt of
                PEG.AltSeq -> do
                    mark1 <- lift Scanner.getMark
                    saveEnterActionResult pos0 mark1 v alt args
                    modify' \ctx -> ctx
                        {
                            ctxState = enterSn
                        }
                    pure ContParse
                PEG.AltAnd -> do
                    seekToMark mark0
                    saveEnterActionResult pos0 mark0 v alt args
                    modify' \ctx -> ctx
                        {
                            ctxState = enterSn
                        }
                    pure ContParse
                PEG.AltNot ->
                    pure CantContParse

parseFail :: forall p e m. Scanner.T p e m => RunT p e m RunningResult
parseFail = do
    stack <- ctxItemStack <$> get
    go stack
    where
        go :: [Item p] -> RunT p e m RunningResult
        go = \case
            [] ->
                pure CantContParse
            item:rest -> case item of
                ItemBackpoint p backSn -> do
                    modify' \ctx -> ctx
                        {
                            ctxState = backSn,
                            ctxItemStack = rest
                        }
                    seekToMark p
                    pure ContParse
                ItemHandleNot alt ->
                    goHandleNot alt rest
                _ ->
                    go rest

        goHandleNot alt = \case
            [] ->
                pure CantContParse
            item:rest -> case item of
                ItemEnter pos0 mark0 v enterSn -> do
                    modify' \ctx -> ctx
                        {
                            ctxItemStack = rest
                        }
                    goEnter alt pos0 mark0 v enterSn
                _ ->
                    goHandleNot alt rest

        goEnter :: Parser.AltNum -> Position -> p -> Parser.VarNum -> Parser.StateNum
            -> RunT p e m RunningResult
        goEnter alt pos0 mark0 v enterSn = do
            parser <- ctxParser <$> get
            case Parser.parserAltKind parser alt of
                PEG.AltSeq ->
                    pure CantContParse
                PEG.AltAnd ->
                    pure CantContParse
                PEG.AltNot -> do
                    seekToMark mark0
                    saveEnterActionResult pos0 mark0 v alt HList.HNil
                    modify' \ctx -> ctx
                        {
                            ctxState = enterSn
                        }
                    pure ContParse

saveEnterActionResult :: Monad m
    => Position -> p -> Parser.VarNum -> Parser.AltNum -> HList.T us
    -> RunT p e m ()
saveEnterActionResult pos0 mark0 v alt args = do
    ctx <- get
    let parser = ctxParser ctx
    let res = Parser.runAction
            do Parser.parserActions parser alt
            do args
    let pos1 = ctxCurrentPosition ctx
    let memoItem = MemoItem pos1 mark0 res
    put do
        ctx
            {
                ctxMemoTable = AlignableMap.insert pos0
                    do case AlignableMap.lookup pos0 do ctxMemoTable ctx of
                        Nothing -> AlignableMap.singleton v memoItem
                        Just vm -> AlignableMap.insert v memoItem vm
                    do ctxMemoTable ctx
            }
    pushItem do ItemArgument res

consumeIfNeeded :: Scanner.T p e m => RunT p e m (Maybe (Parser.TokenNum, e))
consumeIfNeeded = ctxLookAHeadToken <$> get >>= \case
    mtok@(Just _) ->
        pure mtok
    Nothing -> lift Scanner.consumeInput >>= \case
        Nothing -> do
            modify' \ctx -> ctx
                {
                    ctxLookAHeadToken = Nothing
                }
            pure Nothing
        Just tok -> do
            parser <- ctxParser <$> get
            let tokNum = Parser.parserGetTokenNum parser tok
            let r = Just (tokNum, tok)
            modify' \ctx -> ctx
                { ctxCurrentPosition = Alignable.nextAlign do ctxCurrentPosition ctx
                , ctxLookAHeadToken = r
                }
            pure r

seekToMark :: Scanner.T p e m => p -> RunT p e m ()
seekToMark p = do
    modify' \ctx -> ctx
        {
            ctxLookAHeadToken = Nothing
        }
    lift do Scanner.seekToMark p

pushItem :: Monad m => Item p -> RunT p e m ()
pushItem item = modify' \ctx -> ctx
    {
        ctxItemStack = item:ctxItemStack ctx
    }
