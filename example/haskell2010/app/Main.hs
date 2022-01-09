module Main where

import qualified Data.Text as Text
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
        x:_ ->
            pure x
    s <- Text.readFile f
    toks <- case Lexer.lexText s of
        Left msg -> do
            putStrLn "error: "
            putStrLn msg
            System.exitFailure
        Right xs ->
            pure xs
    print ("Token", toks)
    print ("ParseResult", Parser.parseModule toks)
