module Language.Parser.Ptera.Runner.RunT where

import           Language.Parser.Ptera.Prelude

import qualified Data.IntMap.Strict                       as IntMap
import qualified Language.Parser.Ptera.Data.Alignable     as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Map as AlignableMap
import qualified Language.Parser.Ptera.Machine.PEG        as PEG
import qualified Language.Parser.Ptera.Runner.Parser      as Parser
import qualified Language.Parser.Ptera.Scanner            as Scanner
import qualified Unsafe.Coerce                            as Unsafe

type T = RunT

newtype RunT feed posMark elem m a = RunT
    {
        unRunT :: StateT (Context feed posMark elem) m a
    }
    deriving Functor
    deriving (Applicative, Monad) via (StateT (Context feed posMark elem) m)

instance MonadTrans (RunT feed posMark elem) where
    lift mx = RunT do lift mx

runT :: forall feed posMark elem m a. Scanner.T feed posMark elem m
    => RunT feed posMark elem m (Result a)
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
    goResult tok = if
        | tok >= 0 ->
            pure ParseFail
        | otherwise ->
            ctxItemStack <$> get >>= \case
                [ItemArgument x] ->
                    pure do Parsed do Unsafe.unsafeCoerce x
                _ ->
                    pure ParseFail

data Result a
    = Parsed a
    | ParseFail
    deriving (Eq, Show, Functor)

data Context feed posMark elem = Context
    {
        ctxParser         :: Parser.T feed elem,
        ctxState          :: Parser.StateNum,
        ctxItemStack      :: [Item posMark],
        ctxLookAHeadToken :: Maybe (Position, posMark, Parser.TokenNum, Maybe elem),
        ctxNextPosition   :: Position,
        ctxMemoTable      :: AlignableMap.T Position (IntMap.IntMap (MemoItem posMark)),
        ctxNeedBackItemsCount :: Int
    }

newtype Position = Position Int
    deriving (Eq, Show)
    deriving Alignable.T via Alignable.Inst

data MemoItem posMark where
    MemoItemParsed :: Position -> posMark -> a -> MemoItem posMark
    MemoItemFailed :: MemoItem posMark

data Item posMark where
    ItemEnter :: Position -> Maybe posMark -> Parser.VarNum -> Parser.StateNum -> Item posMark
    ItemHandleNot :: Parser.AltNum -> Item posMark
    ItemBackpoint :: Position -> posMark -> Parser.StateNum -> Item posMark
    ItemArgument :: a -> Item posMark

data RunningResult
    = ContParse
    | CantContParse
    deriving (Eq, Show)

initialContext :: Parser.T feed elem -> Parser.StartNum -> Maybe (Context feed posMark elem)
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

transByInput :: forall feed posMark elem m. Scanner.T feed posMark elem m
    => Parser.TokenNum -> RunT feed posMark elem m RunningResult
transByInput tok = go where
    go = do
        ctx0 <- get
        let trans1 = Parser.parserTrans
                do ctxParser ctx0
                do ctxState ctx0
                do tok
        put do ctx0 { ctxState = Parser.transState trans1 }
        let ops = Parser.transOps trans1
        goTransOps ops

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
        runEnter v needBack enterSn
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

runEnter :: Scanner.T p e m
    => Parser.VarNum -> Bool -> Parser.StateNum -> RunT p e m RunningResult
runEnter v needBack enterSn = do
    (pos0, mark0) <- getCurrentPosition
    memoTable <- ctxMemoTable <$> get
    let vm = case AlignableMap.lookup pos0 memoTable of
            Nothing -> IntMap.empty
            Just m  -> m
    case IntMap.lookup v vm of
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

runAction :: Scanner.T feed posMark elem m
    => Position -> Parser.VarNum -> Parser.AltNum -> [u]
    -> RunT feed posMark elem m ()
runAction pos0 v alt args = do
    parser <- getCtx ctxParser
    let ActionResult feedOpt res = Parser.runAction
            do Parser.parserAction parser alt
            do args
    case feedOpt of
        Nothing ->
            pure ()
        Just feed ->
            lift do Scanner.feedback feed
    insertMemoItemIfNeeded v pos0 do
        (pos1, pm1) <- getCurrentPosition
        pure do MemoItemParsed pos1 pm1 res
    pushItem do ItemArgument res

saveFailedEnterAction :: Scanner.T feed posMark elem m
    => Parser.VarNum -> Position -> RunT feed posMark elem m ()
saveFailedEnterAction v pos = insertMemoItemIfNeeded v pos do
    pure MemoItemFailed

insertMemoItemIfNeeded :: Monad m
    => Parser.VarNum -> Position -> RunT feed posMark elem m (MemoItem posMark)
    -> RunT feed posMark elem m ()
insertMemoItemIfNeeded v pos mitem = do
    needBack <- isNeedBack
    when needBack do
        memoItem <- mitem
        RunT do
            modify' \ctx -> ctx
                { ctxMemoTable = AlignableMap.insert pos
                    do case AlignableMap.lookup pos do ctxMemoTable ctx of
                        Nothing -> IntMap.singleton v memoItem
                        Just vm -> IntMap.insert v memoItem vm
                    do ctxMemoTable ctx
                }

getCtx :: Monad m
    => (Context feed posMark elem -> a) -> RunT feed posMark elem m a
getCtx f = RunT do f <$> get
{-# INLINE getCtx #-}

getCurrentPosition :: Scanner.T feed posMark elem m
    => RunT feed posMark elem m (Position, posMark)
getCurrentPosition = getCtx ctxLookAHeadToken >>= \case
    Just (pos, pm, _, _) ->
        pure (pos, pm)
    Nothing -> do
        pm <- lift Scanner.getMark
        pos <- getCtx ctxNextPosition
        pure (pos, pm)

consumeIfNeeded :: Scanner.T feed posMark elem m
    => RunT feed posMark elem m (Parser.TokenNum, Maybe elem)
consumeIfNeeded = getCtx ctxLookAHeadToken >>= \case
    Just (_, _, tn, mt) ->
        pure (tn, mt)
    Nothing -> do
        pm <- lift Scanner.getMark
        r@(tn, mt) <- lift Scanner.consumeInput >>= \case
            Nothing ->
                pure (Parser.eosToken, Nothing)
            Just t -> do
                parser <- getCtx ctxParser
                let tn = Parser.parserGetTokenNum parser t
                pure (tn, Just t)
        RunT do
            modify' \ctx -> ctx
                { ctxNextPosition = Alignable.nextAlign
                    do ctxNextPosition ctx
                , ctxLookAHeadToken = Just
                    (ctxNextPosition ctx, pm, tn, mt)
                }
        pure r

shift :: Monad m => RunT feed posMark elem m ()
shift = getCtx ctxLookAHeadToken >>= \case
    Nothing ->
        error "Must consume before shift"
    Just (_, _, _, Nothing) ->
        error "No more shift"
    Just (_, _, _, Just{}) ->
        RunT do
            modify' \ctx -> ctx
                {
                    ctxLookAHeadToken = Nothing
                }

seekToMark :: Scanner.T feed posMark elem m
    => Position -> posMark -> RunT feed posMark elem m ()
seekToMark pos pm = do
    RunT do
        modify' \ctx -> ctx
            { ctxLookAHeadToken = Nothing
            , ctxNextPosition = pos
            }
    lift do Scanner.seekToPosMark pm

isNeedBack :: Monad m => RunT feed posMark elem m Bool
isNeedBack = do
    needBackItemsCount <- getCtx ctxNeedBackItemsCount
    pure do needBackItemsCount > 0

pushItem :: Scanner.T feed posMark elem m
    => Item posMark -> RunT feed posMark elem m ()
pushItem item = do
    (pos, p) <- getCurrentPosition
    bc0 <- getCtx ctxNeedBackItemsCount
    let bc1 = if isNeedBackItem item then bc0 + 1 else bc0
    when do bc0 == 0 && bc1 > 0
        do lift do Scanner.scanMode do Scanner.ScanModeNeedBack p
    RunT do
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

popItem :: Scanner.T feed posMark elem m => RunT feed posMark elem m (Maybe (Item posMark))
popItem = getCtx ctxItemStack >>= \case
    [] ->
        pure Nothing
    item:rest -> do
        bc0 <- getCtx ctxNeedBackItemsCount
        let bc1 = if isNeedBackItem item then bc0 - 1 else bc0
        when do bc1 == 0
            do lift do Scanner.scanMode Scanner.ScanModeNoBack
        RunT do
            modify' \ctx -> ctx
                { ctxItemStack = rest
                , ctxNeedBackItemsCount = bc1
                }
        pure do Just item

isNeedBackItem :: Item posMark -> Bool
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
