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

runEnter :: Scanner.T p e m => Parser.VarNum -> Parser.StateNum -> RunT p e m RunningResult
runEnter v enterSn = do
    (pos0, mark0) <- getCurrentPosition
    memoTable <- ctxMemoTable <$> get
    let vm = case AlignableMap.lookup pos0 memoTable of
            Nothing -> AlignableMap.empty
            Just m  -> m
    case AlignableMap.lookup v vm of
        Just (MemoItem pos1 mark1 x) -> do
            modify' do \ctx -> ctx { ctxState = enterSn }
            pushItem do ItemArgument x
            seekToMark pos1 mark1
            pure ContParse
        Nothing -> do
            pushItem do ItemEnter pos0 mark0 v enterSn
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
                    seekToMark pos0 mark0
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
                ItemBackpoint pos p backSn -> do
                    modify' \ctx -> ctx
                        {
                            ctxState = backSn,
                            ctxItemStack = rest
                        }
                    seekToMark pos p
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
                    seekToMark pos0 mark0
                    saveEnterActionResult pos0 mark0 v alt HList.HNil
                    modify' \ctx -> ctx
                        {
                            ctxState = enterSn
                        }
                    pure ContParse

saveEnterActionResult :: Scanner.T p e m
    => Position -> p -> Parser.VarNum -> Parser.AltNum -> HList.T us
    -> RunT p e m ()
saveEnterActionResult pos0 mark0 v alt args = do
    ctx <- get
    let parser = ctxParser ctx
    let res = Parser.runAction
            do Parser.parserActions parser alt
            do args
    (pos1, _) <- getCurrentPosition
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
                ( ctxNextPosition ctx
                , p
                , tn
                , mt
                )
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

pushItem :: Monad m => Item p -> RunT p e m ()
pushItem item = modify' \ctx -> ctx
    {
        ctxItemStack = item:ctxItemStack ctx
    }
