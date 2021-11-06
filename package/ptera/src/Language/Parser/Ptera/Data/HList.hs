module Language.Parser.Ptera.Data.HList where

import           Language.Parser.Ptera.Prelude

type T = HList

data HList :: [Type] -> Type where
    HNil :: HList '[]
    (:*) :: x -> HList xs -> HList (x ': xs)

infixr 6 :*
