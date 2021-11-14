module Language.Parser.Ptera.Data.IntMap.GreaterRestriction (
    restrictGreater,
) where

import           Language.Parser.Ptera.Prelude

import           Data.IntMap.Internal


restrictGreater :: Key -> IntMap a -> IntMap a
restrictGreater k t = case t of
    Bin _ m l r | m < 0 ->
        if k >= 0 -- handle negative numbers.
            then go k l
            else l `union` go k r
    _ ->
        go k t
    where
        go k' t' = case t' of
            Bin p m l r
                | nomatch k' p m ->
                    if k' > p
                        then Nil
                        else t'
                | zero k' m ->
                    go k' l `union` r
                | otherwise ->
                    go k' r
            Tip ky _
                | k' > ky ->
                    Nil
                | k' < ky ->
                    t'
                | otherwise ->
                    Nil
            Nil ->
                Nil
