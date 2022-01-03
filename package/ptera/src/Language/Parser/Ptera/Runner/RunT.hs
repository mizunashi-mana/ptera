module Language.Parser.Ptera.Runner.RunT where

import           Language.Parser.Ptera.Prelude

import qualified Data.IntMap.Strict                       as IntMap
import qualified Language.Parser.Ptera.Data.Alignable     as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Map as AlignableMap
import qualified Language.Parser.Ptera.Machine.PEG        as PEG
import qualified Language.Parser.Ptera.Runner.Parser      as Parser
import qualified Language.Parser.Ptera.Scanner            as Scanner
import qualified Language.Parser.Ptera.Syntax             as Syntax
import qualified Unsafe.Coerce                            as Unsafe

type T = RunT

newtype RunT ctx posMark elem m a = RunT
    {
        unRunT :: StateT (Context ctx posMark elem) m a
    }
    deriving Functor
    deriving (Applicative, Monad) via (StateT (Context ctx posMark elem) m)

instance MonadTrans (RunT ctx posMark elem) where
    lift mx = RunT do lift mx

runT :: forall ctx posMark elem m a. Scanner.T posMark elem m
    => RunT ctx posMark elem m (Result a)
runT = go where
    go = do
        (tok, _) <- consumeIfNeeded
        sn <- getCtx ctxState
        if sn < 0
            then goResult tok
            else transByInput tok >>= \case
                ContParse ->
                    go
                CantContParse ->
                    pure ParseFail

    goResult :: Parser.TokenNum -> RunT ctx posMark elem m (Result a)
    goResult tok = if
        | tok >= 0 ->
            pure ParseFail
        | otherwise ->
            getCtx ctxItemStack >>= \case
                [ItemArgument x] ->
                    pure do Parsed do Unsafe.unsafeCoerce x
                _ ->
                    pure ParseFail

data Result a
    = Parsed a
    | ParseFail
    deriving (Eq, Show, Functor)

data Context ctx posMark elem = Context
    {
        ctxParser             :: Parser.T ctx elem,
        ctxState              :: Parser.StateNum,
        ctxItemStack          :: [Item posMark],
        ctxLookAHeadToken     :: Maybe (Position, posMark, Parser.TokenNum, Maybe elem),
        ctxNextPosition       :: Position,
        ctxMemoTable          :: AlignableMap.T Position (IntMap.IntMap (MemoItem posMark)),
        ctxNeedBackItemsCount :: Int,
        ctxCustomContext      :: ctx
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

initialContext :: Parser.T ctx elem -> ctx -> Parser.StartNum -> Maybe (Context ctx posMark elem)
initialContext parser ctx0 s0 = do
    sn0 <- Parser.parserInitial parser s0
    pure do
        Context
            {
                ctxParser = parser,
                ctxState = sn0,
                ctxLookAHeadToken = Nothing,
                ctxItemStack = [],
                ctxNextPosition = Alignable.initialAlign,
                ctxMemoTable = AlignableMap.empty,
                ctxNeedBackItemsCount = 0,
                ctxCustomContext = ctx0
            }

transByInput :: forall ctx posMark elem m. Scanner.T posMark elem m
    => Parser.TokenNum -> RunT ctx posMark elem m RunningResult
transByInput tok = go where
    go = do
        parser <- getCtx ctxParser
        sn0 <- getCtx ctxState
        let trans1 = Parser.parserTrans parser sn0 tok
        setNextState do Parser.transState trans1
        let ops = Parser.transOps trans1
        goTransOps ops

    goTransOps :: [Parser.TransOp] -> RunT ctx posMark elem m RunningResult
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

runTransOp :: Scanner.T posMark elem m
    => Parser.TransOp -> RunT ctx posMark elem m RunningResult
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

runEnter :: Scanner.T posMark elem m
    => Parser.VarNum -> Bool -> Parser.StateNum -> RunT ctx posMark elem m RunningResult
runEnter v needBack enterSn = do
    (pos0, mark0) <- getCurrentPosition
    memoTable <- getCtx ctxMemoTable
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
                setNextState enterSn
                pushItem do ItemArgument x
                seekToMark pos1 mark1
                pure ContParse
            MemoItemFailed ->
                parseFail

runReduce :: forall ctx posMark elem m. Scanner.T posMark elem m
    => Parser.AltNum -> RunT ctx posMark elem m RunningResult
runReduce alt = go [] where
    go :: [u] -> RunT ctx posMark elem m RunningResult
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

    goEnter :: [u] -> Position -> Maybe posMark -> Parser.VarNum -> Parser.StateNum
        -> RunT ctx posMark elem m RunningResult
    goEnter args pos0 mmark0 v enterSn = do
        parser <- getCtx ctxParser
        case Parser.parserAltKind parser alt of
            PEG.AltSeq -> runActionAndSaveEnterResult v pos0 alt args >>= \case
                False ->
                    parseFail
                True -> do
                    setNextState enterSn
                    pure ContParse
            PEG.AltAnd -> runActionAndSaveEnterResult v pos0 alt args >>= \case
                False ->
                    parseFail
                True -> do
                    let mark0 = case mmark0 of
                            Nothing ->
                                error "unreachable: no mark with and alternative"
                            Just x ->
                                x
                    seekToMark pos0 mark0
                    setNextState enterSn
                    pure ContParse
            PEG.AltNot ->
                pure CantContParse

parseFail :: forall ctx posMark elem m. Scanner.T posMark elem m
    => RunT ctx posMark elem m RunningResult
parseFail = go where
    go :: RunT ctx posMark elem m RunningResult
    go = popItem >>= \case
        Nothing ->
            pure CantContParse
        Just item -> case item of
            ItemBackpoint pos p backSn -> do
                setNextState backSn
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

    goEnter :: Parser.AltNum -> Position -> Maybe posMark -> Parser.VarNum -> Parser.StateNum
        -> RunT ctx posMark elem m RunningResult
    goEnter alt pos0 mmark0 v enterSn = do
        parser <- getCtx ctxParser
        case Parser.parserAltKind parser alt of
            PEG.AltSeq ->
                error "unreachable: a not handling with seq alternative"
            PEG.AltAnd ->
                error "unreachable: a not handling with and alternative"
            PEG.AltNot -> runActionAndSaveEnterResult v pos0 alt [] >>= \case
                False ->
                    go
                True -> do
                    let mark0 = case mmark0 of
                            Nothing ->
                                error "unreachable: no mark with not alternative"
                            Just x ->
                                x
                    seekToMark pos0 mark0
                    setNextState enterSn
                    pure ContParse

runActionAndSaveEnterResult :: Scanner.T posMark elem m
    => Parser.VarNum -> Position -> Parser.AltNum -> [u] -> RunT ctx posMark elem m Bool
runActionAndSaveEnterResult v pos0 alt args = runAction alt args >>= \case
    Syntax.ActionTaskFail -> do
        saveFailedEnterAction v pos0
        pure False
    Syntax.ActionTaskResult res -> do
        saveParsedEnterAction v pos0 Nothing res
        pure True
    Syntax.ActionTaskModifyResult ctx1 res -> do
        saveParsedEnterAction v pos0 (Just ctx1) res
        pure True

runAction :: Scanner.T posMark elem m
    => Parser.AltNum -> [u]
    -> RunT ctx posMark elem m (Syntax.ActionTaskResult ctx a)
runAction alt args = do
    parser <- getCtx ctxParser
    ctx0 <- getCtx ctxCustomContext
    let actionTask = Parser.runActionM
            do Parser.parserAction parser alt
            do args
    pure do Syntax.runActionTask actionTask ctx0

saveParsedEnterAction :: Scanner.T posMark elem m
    => Parser.VarNum -> Position -> Maybe ctx -> a
    -> RunT ctx posMark elem m ()
saveParsedEnterAction v pos0 mctx1 res = do
    forM_ mctx1 \ctx1 -> updateCustomContext ctx1
    insertMemoItemIfNeeded v pos0 do
        (pos1, pm1) <- getCurrentPosition
        pure do MemoItemParsed pos1 pm1 res
    pushItem do ItemArgument res

saveFailedEnterAction :: Scanner.T posMark elem m
    => Parser.VarNum -> Position -> RunT ctx posMark elem m ()
saveFailedEnterAction v pos = insertMemoItemIfNeeded v pos do
    pure MemoItemFailed

insertMemoItemIfNeeded :: Monad m
    => Parser.VarNum -> Position -> RunT ctx posMark elem m (MemoItem posMark)
    -> RunT ctx posMark elem m ()
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

updateCustomContext :: Monad m => ctx -> RunT ctx posMark elem m ()
updateCustomContext customCtx = RunT do
    modify' \ctx -> ctx
        {
            ctxMemoTable = AlignableMap.empty,
            ctxCustomContext = customCtx
        }

setNextState :: Monad m => Parser.StateNum -> RunT ctx posMark elem m ()
setNextState sn = RunT do
    modify' \ctx -> ctx
        {
            ctxState = sn
        }

getCtx :: Monad m
    => (Context ctx posMark elem -> a) -> RunT ctx posMark elem m a
getCtx f = RunT do f <$> get
{-# INLINE getCtx #-}

getCurrentPosition :: Scanner.T posMark elem m
    => RunT ctx posMark elem m (Position, posMark)
getCurrentPosition = getCtx ctxLookAHeadToken >>= \case
    Just (pos, pm, _, _) ->
        pure (pos, pm)
    Nothing -> do
        pm <- lift Scanner.getPosMark
        pos <- getCtx ctxNextPosition
        pure (pos, pm)

consumeIfNeeded :: Scanner.T posMark elem m
    => RunT ctx posMark elem m (Parser.TokenNum, Maybe elem)
consumeIfNeeded = getCtx ctxLookAHeadToken >>= \case
    Just (_, _, tn, mt) ->
        pure (tn, mt)
    Nothing -> do
        pm <- lift Scanner.getPosMark
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

shift :: Monad m => RunT ctx posMark elem m ()
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

seekToMark :: Scanner.T posMark elem m
    => Position -> posMark -> RunT ctx posMark elem m ()
seekToMark pos pm = do
    RunT do
        modify' \ctx -> ctx
            { ctxLookAHeadToken = Nothing
            , ctxNextPosition = pos
            }
    lift do Scanner.seekToPosMark pm

isNeedBack :: Monad m => RunT ctx posMark elem m Bool
isNeedBack = do
    needBackItemsCount <- getCtx ctxNeedBackItemsCount
    pure do needBackItemsCount > 0

pushItem :: Scanner.T posMark elem m
    => Item posMark -> RunT ctx posMark elem m ()
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

popItem :: Scanner.T posMark elem m => RunT ctx posMark elem m (Maybe (Item posMark))
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
