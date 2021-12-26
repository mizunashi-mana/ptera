module Language.Parser.Ptera.Runner (
    T,

    RunnerM (..),
    RunT.Result (..),
    runParserM,
    runParser,
) where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Data.Member   as Member
import qualified Language.Parser.Ptera.Data.Record   as Record
import qualified Language.Parser.Ptera.Data.TypeOps  as TypeOps
import qualified Language.Parser.Ptera.Runner.Parser as Parser
import qualified Language.Parser.Ptera.Runner.RunT   as RunT
import qualified Language.Parser.Ptera.Scanner       as Scanner

type T = RunnerM

newtype RunnerM ctx vars rules elem = UnsafeRunnerM (Parser.T ctx elem)
type Runner = RunnerM ()

runParserM :: forall v vars rules ctx posMark elem m a.
    a ~ TypeOps.FromJust (Record.RecordIndex v rules)
    => Member.T v vars
    => Scanner.T posMark elem m
    => Proxy v -> RunnerM ctx vars rules elem -> ctx -> m (RunT.Result a)
runParserM Proxy (UnsafeRunnerM p) customCtx0 =
    case RunT.initialContext p customCtx0 pos of
        Nothing ->
            pure do RunT.ParseFail
        Just initialCtx ->
            evalStateT runner initialCtx
    where
        runner :: StateT (RunT.Context ctx posMark elem) m (RunT.Result a)
        runner = RunT.unRunT RunT.runT

        pos = Member.position
            do proxy# :: Proxy# v
            do proxy# :: Proxy# vars

runParser :: forall v vars rules posMark elem m a.
    a ~ TypeOps.FromJust (Record.RecordIndex v rules)
    => Member.T v vars
    => Scanner.T posMark elem m
    => Proxy v -> Runner vars rules elem -> m (RunT.Result a)
runParser p r = runParserM p r ()
