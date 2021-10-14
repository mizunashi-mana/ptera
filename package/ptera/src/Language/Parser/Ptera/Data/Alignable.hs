module Language.Parser.Ptera.Data.Alignable (
    T,
    Alignable (..),
    Inst (..),
) where

import           Language.Parser.Ptera.Prelude


type T = Alignable

class Coercible Int i => Alignable i where
    initialAlign :: i
    initialAlign = coerce (0 :: Int)

    nextAlign :: i -> i
    nextAlign = coerce (succ :: Int -> Int)

newtype Inst = Inst Int

instance Alignable Inst
