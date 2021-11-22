module Language.Parser.Ptera.Runner (
    T,

    Runner (..),
    RunT.Result (..),
    runParser,
) where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Data.Member   as Member
import qualified Language.Parser.Ptera.Data.Record   as Record
import qualified Language.Parser.Ptera.Data.TypeOps  as TypeOps
import qualified Language.Parser.Ptera.Runner.Parser as Parser
import qualified Language.Parser.Ptera.Runner.RunT   as RunT
import qualified Language.Parser.Ptera.Scanner       as Scanner

type T = Runner

newtype Runner s h e = UnsafeRunner (Parser.T e)

runParser :: forall v s h p e m a. a ~ TypeOps.FromJust (Record.RecordIndex v h)
    => Member.T v s
    => Scanner.T p e m
    => Proxy v -> Runner s h e -> m (RunT.Result a)
runParser Proxy (UnsafeRunner p) = case RunT.initialContext p pos of
    Nothing ->
        pure do RunT.ParseFail
    Just initialCtx ->
        evalStateT RunT.runT initialCtx
    where
        pos = Member.position
            do proxy# :: Proxy# v
            do proxy# :: Proxy# s
