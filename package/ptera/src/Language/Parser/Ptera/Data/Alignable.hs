module Language.Parser.Ptera.Data.Alignable (
    T,
    Alignable,
    initialAlign,
    nextAlign,
) where

import           Language.Parser.Ptera.Prelude


type T = Alignable

type Alignable = Coercible Int

initialAlign :: Alignable i => i
initialAlign = coerce (0 :: Int)

nextAlign :: Alignable i => i -> i
nextAlign = coerce (succ :: Int -> Int)
