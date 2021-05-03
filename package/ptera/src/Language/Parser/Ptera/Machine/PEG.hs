module Language.Parser.Ptera.Machine.PEG where

import Language.Parser.Ptera.Prelude


data PEG n a
    = Epsilon
    | Terminal a
    | NonTerminal n
    | PEG n a :^: PEG n a
    | PEG n a :/: PEG n a
    | Many (PEG n a)
    | And (PEG n a)
    | Not (PEG n a)
    deriving (Eq, Show)

instance Semigroup (PEG n a) where
    (<>) = (:^:)

instance Monoid (PEG n a) where
    mempty = Epsilon

{-# INLINE orP #-}
orP :: [PEG n a] -> PEG n a
orP = \case
    []      -> Epsilon
    g:gs    -> foldr (:/:) g gs

maybeP :: PEG n a -> PEG n a
maybeP g = orP [g, Epsilon]

someP :: PEG n a -> PEG n a
someP g = g <> Many g

manyP :: PEG n a -> PEG n a
manyP g = Many g
