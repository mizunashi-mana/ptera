module Main where

import qualified Data.ByteString    as ByteString
import qualified Lexer
import qualified Parser
import qualified System.Environment as System
import qualified System.Exit        as System

main :: IO ()
main = do
    args <- System.getArgs
    f <- case args of
        [] -> do
            putStrLn "need input path"
            System.exitFailure
        x:_ -> pure x
    s <- ByteString.readFile f
    toks <- case Lexer.lexByteString s of
        Left msg -> do
            putStrLn "error: "
            putStrLn msg
            System.exitFailure
        Right xs -> pure xs
    print ("Token", toks)
    print ("ParseResult", Parser.parseExpr toks)
