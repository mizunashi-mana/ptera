module Language.Parser.Ptera.Data.HList where

type T = HList

data HList us where
    HNil :: HList '[]
    (:*) :: u -> HList us -> HList (u ': us)

infixr 6 :*
