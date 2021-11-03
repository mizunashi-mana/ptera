module Language.Parser.Ptera.Runner.RunT where

import Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Runner.Parser as Parser
import qualified Language.Parser.Ptera.Scanner as Scanner
import qualified Language.Parser.Ptera.Data.HList as HList
import qualified Language.Parser.Ptera.Machine.PEG as PEG


type RunT s p e = StateT (Context s p e)

runT :: forall s p e m. Scanner.T p e m => RunT s p e m FinalResult
runT = go where
    go = consumeIfNeeded >>= \case
        Nothing -> transByInput Parser.eosToken >>= \case
            ContParse ->
                goResult
            CantContParse ->
                pure ParseFail
        Just (tok, _) -> transByInput tok >>= \case
            ContParse ->
                go
            CantContParse ->
                pure ParseFail

    goResult :: RunT s p e m FinalResult
    goResult = get >>= \ctx -> case ctxItemStack ctx of
        [x] -> pure do Parsed x
        _   -> pure ParseFail

data FinalResult where
    Parsed :: a -> FinalResult
    ParseFail :: FinalResult

data Context s p e = Context
    {
        ctxParser :: Parser.T s e,
        ctxState :: Parser.StateNum,
        ctxItemStack :: [Item p],
        ctxLookAHeadToken :: Maybe (Parser.TokenNum, e),
        ctxMemoTable :: () -- TODO
    }

data Item p where
    ItemEnter :: p -> Parser.VarNum -> Parser.StateNum -> Item p
    ItemHandleNot :: p -> Parser.AltNum -> Item p
    ItemBackpoint :: p -> Parser.StateNum -> Item p
    ItemArgument :: a -> Item p

data ContResult
    = ContParse
    | CantContParse

initialContext :: Enum s => Parser.T s e -> s -> Maybe (Context s p e)
initialContext p s = do
    sn0 <- Parser.parserInitial p s
    pure do
        Context
            {
                ctxParser = p,
                ctxState = sn0,
                ctxLookAHeadToken = Nothing,
                ctxItemStack = [],
                ctxMemoTable = ()
            }

transByInput :: forall s p e m. Scanner.T p e m => Parser.TokenNum -> RunT s p e m ContResult
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

    goTransOps :: [Parser.TransOp] -> RunT s p e m ContResult
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

runTransOp :: Scanner.T p e m => Parser.TransOp ->　RunT s p e m ContResult
runTransOp = \case
    Parser.TransOpEnter v enterSn -> do
        p <- lift Scanner.getMark
        pushItem do ItemEnter p v enterSn
        pure ContParse
    Parser.TransOpPushBackpoint backSn -> do
        p <- lift Scanner.getMark
        pushItem do ItemBackpoint p backSn
        pure ContParse
    Parser.TransOpHandleNot alt -> do
        p <- lift Scanner.getMark
        pushItem do ItemHandleNot p alt
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

runReduce :: forall s p e m. Scanner.T p e m => Parser.AltNum -> RunT s p e m ContResult
runReduce alt = do
        stack0 <- ctxItemStack <$> get
        go HList.HNil stack0
    where
        go :: HList.T us -> [Item p] -> RunT s p e m ContResult
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
                ItemEnter p v enterSn ->
                    goEnter args p v enterSn rest

        goEnter :: HList.T us -> p -> Parser.VarNum -> Parser.StateNum -> [Item p]
            -> RunT s p e m ContResult
        goEnter args p v enterSn rest = do
            modify' \ctx -> ctx
                {
                    ctxItemStack = rest
                }
            saveEnterActionResult v alt args
            parser <- ctxParser <$> get
            case Parser.parserAltKind parser alt of
                PEG.AltSeq -> do
                    modify' \ctx -> ctx
                        {
                            ctxState = enterSn
                        }
                    pure ContParse
                PEG.AltAnd -> do
                    modify' \ctx -> ctx
                        {
                            ctxState = enterSn
                        }
                    seekToMark p
                    pure ContParse
                PEG.AltNot ->
                    pure CantContParse

parseFail :: forall s p e m. Scanner.T p e m => RunT s p e m ContResult
parseFail = do
    stack <- ctxItemStack <$> get
    go stack
    where
        go :: [Item p] -> RunT s p e m ContResult
        go = \case
            [] ->
                pure CantContParse
            ItemBackpoint p backSn:rest -> do
                modify' \ctx -> ctx
                    {
                        ctxState = backSn,
                        ctxItemStack = rest
                    }
                seekToMark p
                pure ContParse
            _:rest ->
                go rest

-- TODO: memorize
saveEnterActionResult :: Monad m => Parser.VarNum -> Parser.AltNum -> HList.T us -> RunT s p e m ()
saveEnterActionResult _ alt args = do
    parser <- ctxParser <$> get
    let item = ItemArgument do
            Parser.runAction
                do Parser.parserActions parser alt
                do args
    pushItem item

consumeIfNeeded :: Scanner.T p e m => RunT s p e m (Maybe (Parser.TokenNum, e))
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
                {
                    ctxLookAHeadToken = r
                }
            pure r

seekToMark :: Scanner.T p e m => p -> RunT s p e m ()
seekToMark p = do
    modify' \ctx -> ctx
        {
            ctxLookAHeadToken = Nothing
        }
    lift do Scanner.seekToMark p

pushItem :: Monad m => Item p -> RunT s p e m ()
pushItem item = modify' \ctx -> ctx
    {
        ctxItemStack = item:ctxItemStack ctx
    }
