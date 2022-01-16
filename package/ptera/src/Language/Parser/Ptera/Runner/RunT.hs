module Language.Parser.Ptera.Runner.RunT (
    T,

    RunT (..),
    runT,

    ParseResult (..),
    Context (..),
    initialContext,
    Position (..),
) where

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

newtype RunT ctx posMark elem altHelp m a = RunT
    {
        unRunT :: StateT (Context ctx posMark elem altHelp) m a
    }
    deriving Functor
    deriving (
        Applicative,
        Monad
    ) via (StateT (Context ctx posMark elem altHelp) m)

instance MonadTrans (RunT ctx posMark elem altHelp) where
    lift mx = RunT do lift mx

runT :: forall ctx posMark elem altHelp m a. Scanner.T posMark elem m
    => RunT ctx posMark elem altHelp m (ParseResult posMark altHelp a)
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
                    goFailed

    goResult
        :: Parser.TokenNum
        -> RunT ctx posMark elem altHelp m (ParseResult posMark altHelp a)
    goResult tok = getCtx ctxItemStack >>= \case
        [ItemArgument x] | tok < 0 ->
            pure do Parsed do Unsafe.unsafeCoerce x
        _ -> do
            if tok >= 0
                then reportError FailedByEarlyParsed
                else reportError FailedByNotEnoughInput
            goFailed

    goFailed :: RunT ctx posMark elem altHelp m (ParseResult posMark altHelp a)
    goFailed = getCtx ctxDeepestError >>= \case
        Just (_, posMark0, failedReason) ->
            pure do ParseFailed posMark0 failedReason
        Nothing ->
            error "unreachable: any errors are available."

data ParseResult posMark altHelp a
    = Parsed a
    | ParseFailed posMark (FailedReason altHelp)
    deriving (Show, Functor)

data FailedReason altHelp
    = FailedWithHelp [(StringLit, Maybe altHelp, Maybe Int)]
    | FailedToStart
    | FailedByEarlyParsed
    | FailedByNotEnoughInput
    deriving (Show, Functor)

data Context ctx posMark elem altHelp = Context
    { ctxParser             :: Parser.T ctx elem altHelp
    , ctxState              :: Parser.StateNum
    , ctxItemStack          :: [Item posMark]
    , ctxLookAHeadToken     :: Maybe (Position, posMark, Parser.TokenNum, Maybe elem)
    , ctxNextPosition       :: Position
    , ctxDeepestError       :: Maybe (Position, posMark, FailedReason altHelp)
    , ctxMemoTable          :: AlignableMap.T Position (IntMap.IntMap (MemoItem posMark))
    , ctxNeedBackItemsCount :: Int
    , ctxCustomContext      :: ctx
    }

newtype Position = Position Int
    deriving (Eq, Ord, Show)
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

initialContext
    :: Parser.T ctx elem altHelp -> ctx -> Parser.StartNum
    -> Maybe (Context ctx posMark elem altHelp)
initialContext parser ctx0 s0 = do
    sn0 <- Parser.parserInitial parser s0
    pure do
        Context
            { ctxParser = parser
            , ctxState = sn0
            , ctxLookAHeadToken = Nothing
            , ctxItemStack = []
            , ctxNextPosition = Alignable.initialAlign
            , ctxMemoTable = AlignableMap.empty
            , ctxNeedBackItemsCount = 0
            , ctxCustomContext = ctx0
            , ctxDeepestError = Nothing
            }

transByInput :: forall ctx posMark elem altHelp m
    .  Scanner.T posMark elem m
    => Parser.TokenNum -> RunT ctx posMark elem altHelp m RunningResult
transByInput tok = go where
    go = do
        parser <- getCtx ctxParser
        sn0 <- getCtx ctxState
        let trans1 = Parser.parserTrans parser sn0 tok
        let sn1 = Parser.transState trans1
        setNextState sn1
        itemStackShow <- prettyShowItemStack
        debugTraceShow ("transByInput", sn0, tok, trans1, itemStackShow) do pure ()
        case Parser.transOps trans1 of
            ops@(_:_) ->
                goTransOps ops
            []
                | sn1 < 0 ->
                    parseFailWithState sn0
                | otherwise ->
                    pure ContParse

    goTransOps :: [Parser.TransOp]
        -> RunT ctx posMark elem altHelp m RunningResult
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

prettyShowItemStack :: Monad m => RunT ctx posMark elem altHelp m [StringLit]
prettyShowItemStack = do
    itemStack <- getCtx ctxItemStack
    pure [ showItem item | item <- itemStack ]
    where
        showItem = \case
            ItemEnter p _ v s ->
                "ItemEnter " <> show (p, v, s)
            ItemHandleNot alt ->
                "ItemHandleNot " <> show alt
            ItemBackpoint p _ s ->
                "ItemBackpoint " <> show (p, s)
            ItemArgument _ ->
                "ItemArgument"

runTransOp :: Scanner.T posMark elem m
    => Parser.TransOp -> RunT ctx posMark elem altHelp m RunningResult
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
            parseFail do Just FailedByNotEnoughInput
        (_, Just x) -> do
            pushItem do ItemArgument x
            shift
            pure ContParse
    Parser.TransOpReduce alt ->
        runReduce alt

runEnter :: Scanner.T posMark elem m
    => Parser.VarNum -> Bool -> Parser.StateNum
    -> RunT ctx posMark elem altHelp m RunningResult
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
                parseFail Nothing

debugShowHelpAlt :: Monad m
    => StringLit -> Parser.AltNum -> RunT ctx posMark elem altHelp m ()
debugShowHelpAlt msg alt = do
    parser <- getCtx ctxParser
    let (dv, _) = Parser.parserAltHelp parser alt
    debugTraceShow (msg, alt, dv) do pure ()

runReduce :: forall ctx posMark elem altHelp m
    .  Scanner.T posMark elem m
    => Parser.AltNum -> RunT ctx posMark elem altHelp m RunningResult
runReduce alt = debugShowHelpAlt "runReduce" alt >> go [] where
    go :: [u] -> RunT ctx posMark elem altHelp m RunningResult
    go args = popItem >>= \case
        Nothing ->
            pure CantContParse
        Just item -> case item of
            ItemArgument x ->
                go do Unsafe.unsafeCoerce x:args
            ItemBackpoint{} ->
                go args
            ItemHandleNot{} ->
                parseFailWithAlt alt
            ItemEnter pos mmark v enterSn ->
                goEnter args pos mmark v enterSn

    goEnter
        :: [u] -> Position -> Maybe posMark -> Parser.VarNum -> Parser.StateNum
        -> RunT ctx posMark elem altHelp m RunningResult
    goEnter args pos0 mmark0 v enterSn = do
        parser <- getCtx ctxParser
        case Parser.parserAltKind parser alt of
            PEG.AltSeq -> runActionAndSaveEnterResult v pos0 alt args >>= \case
                False ->
                    parseFailWithAlt alt
                True -> do
                    setNextState enterSn
                    pure ContParse
            PEG.AltAnd -> runActionAndSaveEnterResult v pos0 alt args >>= \case
                False ->
                    parseFailWithAlt alt
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

parseFailWithAlt :: forall ctx posMark elem altHelp m
    .  Scanner.T posMark elem m
    => Parser.AltNum -> RunT ctx posMark elem altHelp m RunningResult
parseFailWithAlt alt = do
    parser <- getCtx ctxParser
    let (varHelp, altHelp) = Parser.parserAltHelp parser alt
    parseFail do Just do FailedWithHelp [(varHelp, altHelp, Nothing)]

parseFailWithState :: forall ctx posMark elem altHelp m
    .  Scanner.T posMark elem m
    => Parser.StateNum -> RunT ctx posMark elem altHelp m RunningResult
parseFailWithState sn = do
    parser <- getCtx ctxParser
    let altItems = Parser.parserStateHelp parser sn
    let helps =
            [
                ( varHelp
                , altHelp
                , Just pos
                )
            | (alt, pos) <- altItems
            , let (varHelp, altHelp) = Parser.parserAltHelp parser alt
            ]
    parseFail do Just do FailedWithHelp helps

parseFail :: forall ctx posMark elem altHelp m
    .  Scanner.T posMark elem m
    => Maybe (FailedReason altHelp) -> RunT ctx posMark elem altHelp m RunningResult
parseFail = go0 where
    go0 :: Maybe (FailedReason altHelp) -> RunT ctx posMark elem altHelp m RunningResult
    go0 mayFailedReason = do
        debugTraceShow ("parseFail", fmap (const ()) <$> mayFailedReason) do pure ()
        case mayFailedReason of
            Nothing ->
                pure ()
            Just failedReason -> do
                reportError failedReason
        go

    go :: RunT ctx posMark elem altHelp m RunningResult
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

    goEnter
        :: Parser.AltNum -> Position -> Maybe posMark -> Parser.VarNum -> Parser.StateNum
        -> RunT ctx posMark elem altHelp m RunningResult
    goEnter alt pos0 mmark0 v enterSn = do
        parser <- getCtx ctxParser
        case Parser.parserAltKind parser alt of
            PEG.AltSeq ->
                error "unreachable: a not handling with seq alternative"
            PEG.AltAnd ->
                error "unreachable: a not handling with and alternative"
            PEG.AltNot -> runActionAndSaveEnterResult v pos0 alt [] >>= \case
                False ->
                    parseFailWithAlt alt
                True -> do
                    let mark0 = case mmark0 of
                            Nothing ->
                                error "unreachable: no mark with not alternative"
                            Just x ->
                                x
                    seekToMark pos0 mark0
                    setNextState enterSn
                    pure ContParse

runActionAndSaveEnterResult
    :: Scanner.T posMark elem m
    => Parser.VarNum -> Position -> Parser.AltNum -> [u]
    -> RunT ctx posMark elem altHelp m Bool
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
    -> RunT ctx posMark elem altHelp m (Syntax.ActionTaskResult ctx a)
runAction alt args = do
    parser <- getCtx ctxParser
    ctx0 <- getCtx ctxCustomContext
    let actionTask = Parser.runActionM
            do Parser.parserAction parser alt
            do args
    pure do Syntax.runActionTask actionTask ctx0

saveParsedEnterAction
    :: Scanner.T posMark elem m
    => Parser.VarNum -> Position -> Maybe ctx -> a
    -> RunT ctx posMark elem altHelp m ()
saveParsedEnterAction v pos0 mctx1 res = do
    forM_ mctx1 \ctx1 -> updateCustomContext ctx1
    insertMemoItemIfNeeded v pos0 do
        (pos1, pm1) <- getCurrentPosition
        pure do MemoItemParsed pos1 pm1 res
    pushItem do ItemArgument res

saveFailedEnterAction
    :: Monad m
    => Parser.VarNum -> Position -> RunT ctx posMark elem altHelp m ()
saveFailedEnterAction v pos = insertMemoItemIfNeeded v pos do
    pure MemoItemFailed

reportError
    :: Scanner.T posMark elem m
    => FailedReason altHelp -> RunT ctx posMark elem altHelp m ()
reportError failedReason = do
    (pos0, posMark0) <- getCurrentPosition
    RunT do
        modify' \ctx -> ctx
            { ctxDeepestError = case ctxDeepestError ctx of
                oldErr@(Just (pos1, _, _)) | pos0 < pos1 ->
                    oldErr
                _ ->
                    Just (pos0, posMark0, failedReason)
            }

insertMemoItemIfNeeded
    :: Monad m
    => Parser.VarNum -> Position
    -> RunT ctx posMark elem altHelp m (MemoItem posMark)
    -> RunT ctx posMark elem altHelp m ()
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

updateCustomContext :: Monad m => ctx -> RunT ctx posMark elem altHelp m ()
updateCustomContext customCtx = RunT do
    modify' \ctx -> ctx
        {
            ctxMemoTable = AlignableMap.empty,
            ctxCustomContext = customCtx
        }

setNextState :: Monad m => Parser.StateNum -> RunT ctx posMark elem altHelp m ()
setNextState sn = RunT do
    modify' \ctx -> ctx
        { ctxState = sn
        }

getCtx :: Monad m
    => (Context ctx posMark elem altHelp -> a)
    -> RunT ctx posMark elem altHelp m a
getCtx f = RunT do f <$> get
{-# INLINE getCtx #-}

getCurrentPosition :: Scanner.T posMark elem m
    => RunT ctx posMark elem altHelp m (Position, posMark)
getCurrentPosition = getCtx ctxLookAHeadToken >>= \case
    Just (pos, pm, _, _) ->
        pure (pos, pm)
    Nothing -> do
        pm <- lift Scanner.getPosMark
        pos <- getCtx ctxNextPosition
        pure (pos, pm)

consumeIfNeeded :: Scanner.T posMark elem m
    => RunT ctx posMark elem altHelp m (Parser.TokenNum, Maybe elem)
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

shift :: Monad m => RunT ctx posMark elem altHelp m ()
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
    => Position -> posMark -> RunT ctx posMark elem altHelp m ()
seekToMark pos pm = do
    RunT do
        modify' \ctx -> ctx
            { ctxLookAHeadToken = Nothing
            , ctxNextPosition = pos
            }
    lift do Scanner.seekToPosMark pm

isNeedBack :: Monad m => RunT ctx posMark elem altHelp m Bool
isNeedBack = do
    needBackItemsCount <- getCtx ctxNeedBackItemsCount
    pure do needBackItemsCount > 0

pushItem :: Scanner.T posMark elem m
    => Item posMark -> RunT ctx posMark elem altHelp m ()
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

popItem :: Scanner.T posMark elem m
    => RunT ctx posMark elem altHelp m (Maybe (Item posMark))
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
