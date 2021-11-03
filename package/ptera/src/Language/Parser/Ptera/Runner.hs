module Language.Parser.Ptera.Runner where

import qualified Language.Parser.Ptera.Machine.SRB    as SRB
import qualified Language.Parser.Ptera.Runner.Parser  as Parser
import qualified Language.Parser.Ptera.Runner.RunT    as RunT
import qualified Language.Parser.Ptera.Runner.Scanner as Scanner

type T = Parser.T
type Scanner = Scanner.T
type Parser = Parser.T

runParser :: Enum s => Scanner p e m => s -> Parser s -> m Result
runParser = undefined
