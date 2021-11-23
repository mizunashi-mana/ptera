module Language.Parser.Ptera.Runner.RunT where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Data.Alignable     as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Map as AlignableMap
import qualified Language.Parser.Ptera.Machine.PEG        as PEG
import qualified Language.Parser.Ptera.Runner.Parser      as Parser
import qualified Language.Parser.Ptera.Scanner            as Scanner
import qualified Unsafe.Coerce                            as Unsafe

type T p e = RunT p e

type RunT p e = StateT (Context p e)

runT :: forall p e m a. Scanner.T p e m => RunT p e m (Result a)
runT = go where
    go = do
        (tok, _) <- consumeIfNeeded
        sn <- ctxState <$> get
        if sn < 0
            then goResult tok
            else transByInput tok >>= \case
                ContParse ->
                    go
                CantContParse ->
                    pure ParseFail

    goResult :: Parser.TokenNum -> RunT p e m (Result a)
    goResult tok
        | tok >= 0 =
            pure ParseFail
        | otherwise =
            ctxItemStack <$> get >>= \case
                [ItemArgument x] ->
                    pure do Parsed do Unsafe.unsafeCoerce x
                _ ->
                    pure ParseFail

data Result a
    = Parsed a
    | ParseFail
    deriving (Eq, Show, Functor)

data Context p e = Context
    {
        ctxParser         :: Parser.T e,
        ctxState          :: Parser.StateNum,
        ctxItemStack      :: [Item p],
        ctxLookAHeadToken :: Maybe (Position, p, Parser.TokenNum, Maybe e),
        ctxNextPosition   :: Position,
        ctxMemoTable      :: AlignableMap.T Position (AlignableMap.T Parser.VarNum (MemoItem p)),
        ctxNeedBackItemsCount :: Int
    }

newtype Position = Position Int
    deriving (Eq, Show)
    deriving Alignable.T via Alignable.Inst

data MemoItem p where
    MemoItemParsed :: Position -> p -> a -> MemoItem p
    MemoItemFailed :: MemoItem p

data Item p where
    ItemEnter :: Position -> Maybe p -> Parser.VarNum -> Parser.StateNum -> Item p
    ItemHandleNot :: Parser.AltNum -> Item p
    ItemBackpoint :: Position -> p -> Parser.StateNum -> Item p
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
                ctxNextPosition = Alignable.initialAlign,
                ctxMemoTable = AlignableMap.empty,
                ctxNeedBackItemsCount = 0
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
    Parser.TransOpEnter v needBack enterSn ->
        runEnter needBack v enterSn
    Parser.TransOpPushBackpoint backSn -> do
        (pos, mark) <- getCurrentPosition
        pushItem do ItemBackpoint pos mark backSn
        pure ContParse
    Parser.TransOpHandleNot alt -> do
        pushItem do ItemHandleNot alt
        pure ContParse
    Parser.TransOpShift -> consumeIfNeeded >>= \case
        (_, Nothing) ->
            parseFail
        (_, Just x) -> do
            pushItem do ItemArgument x
            shift
            pure ContParse
    Parser.TransOpReduce alt ->
        runReduce alt

runEnter :: Scanner.T p e m => Bool -> Parser.VarNum -> Parser.StateNum -> RunT p e m RunningResult
runEnter needBack v enterSn = do
    (pos0, mark0) <- getCurrentPosition
    memoTable <- ctxMemoTable <$> get
    let vm = case AlignableMap.lookup pos0 memoTable of
            Nothing -> AlignableMap.empty
            Just m  -> m
    case AlignableMap.lookup v vm of
        Nothing -> do
            let mmark0 = if needBack
                    then Just mark0
                    else Nothing
            pushItem do ItemEnter pos0 mmark0 v enterSn
            pure ContParse
        Just memoItem -> case memoItem of
            MemoItemParsed pos1 mark1 x -> do
                modify' do \ctx -> ctx { ctxState = enterSn }
                pushItem do ItemArgument x
                seekToMark pos1 mark1
                pure ContParse
            MemoItemFailed ->
                parseFail

runReduce :: forall p e m. Scanner.T p e m => Parser.AltNum -> RunT p e m RunningResult
runReduce alt = go [] where
    go :: [u] -> RunT p e m RunningResult
    go args = popItem >>= \case
        Nothing ->
            pure CantContParse
        Just item -> case item of
            ItemArgument x ->
                go do Unsafe.unsafeCoerce x:args
            ItemBackpoint{} ->
                go args
            ItemHandleNot{} ->
                parseFail
            ItemEnter pos mmark v enterSn ->
                goEnter args pos mmark v enterSn

    goEnter :: [u] -> Position -> Maybe p -> Parser.VarNum -> Parser.StateNum
        -> RunT p e m RunningResult
    goEnter args pos0 mmark0 v enterSn = do
        parser <- ctxParser <$> get
        case Parser.parserAltKind parser alt of
            PEG.AltSeq -> do
                saveEnterActionResult pos0 v alt args
                modify' \ctx -> ctx
                    {
                        ctxState = enterSn
                    }
                pure ContParse
            PEG.AltAnd -> case mmark0 of
                Nothing ->
                    error "unreachable: no mark with and alternative"
                Just mark0 -> do
                    seekToMark pos0 mark0
                    saveEnterActionResult pos0 v alt args
                    modify' \ctx -> ctx
                        {
                            ctxState = enterSn
                        }
                    pure ContParse
            PEG.AltNot ->
                pure CantContParse

parseFail :: forall p e m. Scanner.T p e m => RunT p e m RunningResult
parseFail = go where
    go :: RunT p e m RunningResult
    go = popItem >>= \case
        Nothing ->
            pure CantContParse
        Just item -> case item of
            ItemBackpoint pos p backSn -> do
                modify' \ctx -> ctx
                    {
                        ctxState = backSn
                    }
                seekToMark pos p
                pure ContParse
            ItemHandleNot alt ->
                goHandleNot alt
            ItemArgument{} ->
                go
            ItemEnter pos0 _ v _ -> do
                saveFailedEnterAction v pos0
                go

    goHandleNot alt = popItem >>= \case
        Nothing ->
            pure CantContParse
        Just item -> case item of
            ItemEnter pos0 mmark0 v enterSn ->
                goEnter alt pos0 mmark0 v enterSn
            _ ->
                goHandleNot alt

    goEnter :: Parser.AltNum -> Position -> Maybe p -> Parser.VarNum -> Parser.StateNum
        -> RunT p e m RunningResult
    goEnter alt pos0 mmark0 v enterSn = do
        parser <- ctxParser <$> get
        case Parser.parserAltKind parser alt of
            PEG.AltSeq ->
                error "unreachable: a not handling with seq alternative"
            PEG.AltAnd ->
                error "unreachable: a not handling with and alternative"
            PEG.AltNot -> case mmark0 of
                Nothing ->
                    error "unreachable: no mark with not alternative"
                Just mark0 -> do
                    seekToMark pos0 mark0
                    saveEnterActionResult pos0 v alt []
                    modify' \ctx -> ctx
                        {
                            ctxState = enterSn
                        }
                    pure ContParse

saveEnterActionResult :: Scanner.T p e m
    => Position -> Parser.VarNum -> Parser.AltNum -> [u]
    -> RunT p e m ()
saveEnterActionResult pos0 v alt args = do
    parser <- ctxParser <$> get
    let res = Parser.runAction
            do Parser.parserAction parser alt
            do args
    needBack <- isNeedBack
    when needBack do
        (pos1, mark1) <- getCurrentPosition
        let memoItem = MemoItemParsed pos1 mark1 res
        modify' \ctx -> ctx
            {
                ctxMemoTable = AlignableMap.insert pos0
                    do case AlignableMap.lookup pos0 do ctxMemoTable ctx of
                        Nothing -> AlignableMap.singleton v memoItem
                        Just vm -> AlignableMap.insert v memoItem vm
                    do ctxMemoTable ctx
            }
    pushItem do ItemArgument res

saveFailedEnterAction :: Scanner.T p e m
    => Parser.VarNum -> Position -> RunT p e m ()
saveFailedEnterAction v pos0 = do
    let memoItem = MemoItemFailed
    needBack <- isNeedBack
    when needBack do
        modify' \ctx -> ctx
            { ctxMemoTable = AlignableMap.insert pos0
                do case AlignableMap.lookup pos0 do ctxMemoTable ctx of
                    Nothing -> AlignableMap.singleton v memoItem
                    Just vm -> AlignableMap.insert v memoItem vm
                do ctxMemoTable ctx
            }

getCurrentPosition :: Scanner.T p e m => RunT p e m (Position, p)
getCurrentPosition = ctxLookAHeadToken <$> get >>= \case
    Just (pos, p, _, _) ->
        pure (pos, p)
    Nothing -> do
        p <- lift Scanner.getMark
        pos <- ctxNextPosition <$> get
        pure (pos, p)

consumeIfNeeded :: Scanner.T p e m => RunT p e m (Parser.TokenNum, Maybe e)
consumeIfNeeded = ctxLookAHeadToken <$> get >>= \case
    Just (_, _, tn, mt) ->
        pure (tn, mt)
    Nothing -> do
        p <- lift Scanner.getMark
        r@(tn, mt) <- lift Scanner.consumeInput >>= \case
            Nothing ->
                pure (Parser.eosToken, Nothing)
            Just t -> do
                parser <- ctxParser <$> get
                let tn = Parser.parserGetTokenNum parser t
                pure (tn, Just t)
        modify' \ctx -> ctx
            { ctxNextPosition = Alignable.nextAlign
                do ctxNextPosition ctx
            , ctxLookAHeadToken = Just
                (ctxNextPosition ctx, p, tn, mt)
            }
        pure r

shift :: Monad m => RunT p e m ()
shift = ctxLookAHeadToken <$> get >>= \case
    Nothing ->
        error "Must consume before shift"
    Just (_, _, _, Nothing) ->
        error "No more shift"
    Just (_, _, _, Just{}) ->
        modify' \ctx -> ctx
            {
                ctxLookAHeadToken = Nothing
            }

seekToMark :: Scanner.T p e m => Position -> p -> RunT p e m ()
seekToMark pos p = do
    modify' \ctx -> ctx
        { ctxLookAHeadToken = Nothing
        , ctxNextPosition = pos
        }
    lift do Scanner.seekToMark p

isNeedBack :: Monad m => RunT p e m Bool
isNeedBack = do
    needBackItemsCount <- ctxNeedBackItemsCount <$> get
    pure do needBackItemsCount > 0

pushItem :: Scanner.T p e m => Item p -> RunT p e m ()
pushItem item = do
    (pos, p) <- getCurrentPosition
    bc0 <- ctxNeedBackItemsCount <$> get
    let bc1 = if isNeedBackItem item then bc0 + 1 else bc0
    when do bc0 == 0 && bc1 > 0
        do lift do Scanner.scanMode do Scanner.ScanModeNeedBack p
    modify' \ctx -> ctx
        { ctxItemStack = item:ctxItemStack ctx
        , ctxNeedBackItemsCount = bc1
        , ctxMemoTable = if bc0 == 0 && bc1 > 0
            then do
                AlignableMap.restrictGreaterOrEqual
                    do pos
                    do ctxMemoTable ctx
            else
                ctxMemoTable ctx
        }

popItem :: Scanner.T p e m => RunT p e m (Maybe (Item p))
popItem = ctxItemStack <$> get >>= \case
    [] ->
        pure Nothing
    item:rest -> do
        bc0 <- ctxNeedBackItemsCount <$> get
        let bc1 = if isNeedBackItem item then bc0 - 1 else bc0
        when do bc1 == 0
            do lift do Scanner.scanMode Scanner.ScanModeNoBack
        modify' \ctx -> ctx
            { ctxItemStack = rest
            , ctxNeedBackItemsCount = bc1
            }
        pure do Just item

isNeedBackItem :: Item p -> Bool
isNeedBackItem = \case
    ItemHandleNot{} ->
        False
    ItemBackpoint{} ->
        True
    ItemEnter _ mmark _ _ -> case mmark of
        Nothing ->
            False
        Just{} ->
            True
    ItemArgument{} ->
        False
