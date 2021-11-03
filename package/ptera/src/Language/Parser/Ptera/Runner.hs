module Language.Parser.Ptera.Runner where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Runner.Parser  as Parser
import qualified Language.Parser.Ptera.Runner.RunT    as RunT
import qualified Language.Parser.Ptera.Scanner as Scanner

type T = Parser.T
type Scanner = Scanner.T
type Parser = Parser.T
type Result = RunT.Result

runParser :: Enum s => Scanner.T p e m => Parser.T s e -> s -> m Result
runParser p s = case RunT.initialContext p s of
    Nothing ->
        pure do RunT.ParseFail
    Just initialCtx ->
        evalStateT RunT.runT initialCtx
