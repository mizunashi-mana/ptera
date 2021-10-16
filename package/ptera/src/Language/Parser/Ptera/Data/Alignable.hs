module Language.Parser.Ptera.Data.Alignable (
    T,
    Alignable (..),
    initialAlign,
    nextAlign,
    Inst (..),
) where

import           Language.Parser.Ptera.Prelude


type T = Alignable

class Coercible Int i => Alignable i

initialAlign :: Alignable i => i
initialAlign = coerce (0 :: Int)

nextAlign :: Alignable i => i -> i
nextAlign = coerce (succ :: Int -> Int)

newtype Inst = Inst Int

instance Alignable Inst
