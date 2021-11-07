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

newtype Runner h e = UnsafeRunner (Parser.T e)

runParser :: forall k h p e m a. a ~ TypeOps.FromJust (Record.RecordIndex k h)
    => Member.T k (TypeOps.MapFst h)
    => Scanner.T p e m
    => Proxy# k -> Runner h e -> m (RunT.Result a)
runParser kp (UnsafeRunner p) = case RunT.initialContext p pos of
    Nothing ->
        pure do RunT.ParseFail
    Just initialCtx ->
        evalStateT RunT.runT initialCtx
    where
        pos = Member.position kp
            do proxy# :: Proxy# (TypeOps.MapFst h)
