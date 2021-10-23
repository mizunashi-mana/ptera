module Language.Parser.Ptera.Data.NonEmptyBag
    (
        T,
        NonEmptyBag (..),
        toNonEmpty,
    ) where

import Language.Parser.Ptera.Prelude

import qualified Data.List.NonEmpty as NonEmpty


type T = NonEmptyBag

data NonEmptyBag a
    = Single a
    | Append (NonEmptyBag a) (NonEmptyBag a)
    | List (NonEmpty a)
    deriving (Show, Functor, Foldable)

instance Applicative NonEmptyBag where
    pure x = Single x

    Single f <*> mx = fmap f mx
    Append fs1 fs2 <*> mx = Append
        do fs1 <*> mx
        do fs2 <*> mx
    List (f0 :| fs) <*> mx = foldl'
        do \rs f -> Append rs
            do fmap f mx
        do fmap f0 mx
        fs

instance Monad NonEmptyBag where
    Single x >>= f = f x
    Append xs1 xs2 >>= f = Append
        do xs1 >>= f
        do xs2 >>= f
    List (x0 :| xs) >>= f = foldl'
        do \rs x -> Append rs
            do f x
        do f x0
        xs

instance Semigroup (NonEmptyBag a) where
    xs1 <> xs2 = Append xs1 xs2

toNonEmpty :: NonEmptyBag a -> NonEmpty a
toNonEmpty xs0 = go xs0 where
    go = \case
        Single x ->
            pure x
        List xs ->
            xs
        Append xs1 xs2 ->
            go2
                do go xs2
                xs1

    go2 accs = \case
        Single x ->
            x NonEmpty.<| accs
        List xs ->
            xs <> accs
        Append xs1 xs2 ->
            go2
                do go2 accs xs2
                xs1
