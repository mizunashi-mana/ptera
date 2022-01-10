module Lexer.PreprocessLayout where

import Types

preprocessLayout :: [(Location, Token)] -> [Token]
preprocessLayout ts0 = case ts0 of
    (loc,t):ts
        | isFirstLayoutToken t ->
            let r' = locRow loc
            in go1 r' ts0
        | otherwise ->
            let r' = locRow loc
                c' = locCol loc
            in TokVirtExpBrace c':TokVirtNewline c':go1 r' ts0
    [] ->
        [TokVirtExpBrace 0]
    where
        go1 r ts = case ts of
            (loc,t):ts' ->
                let r' = locRow loc
                    c' = locCol loc
                in if r /= r'
                    then TokVirtNewline c':go2 r' c' t ts'
                    else go2 r' c' t ts'
            [] ->
                []

        go2 r1 c1 t1 ts
            | isLayoutToken t1 = case ts of
                (loc2,t2):ts' ->
                    let r2 = locRow loc2
                        c2 = locCol loc2
                    in if t2 == TokSpOpenBrace
                        then t1:t2:go1 r2 ts'
                        else t1:TokVirtExpBrace c2:TokVirtNewline c2:t2:go1 r2 ts'
                [] ->
                    [t1, TokVirtExpBrace 0]
            | otherwise =
                t1:go1 r1 ts

isFirstLayoutToken :: Token -> Bool
isFirstLayoutToken t = case t of
    TokKwModule -> True
    _           -> False

isLayoutToken :: Token -> Bool
isLayoutToken t = case t of
    TokKwLet -> True
    TokKwWhere -> True
    TokKwDo -> True
    TokKwOf -> True
