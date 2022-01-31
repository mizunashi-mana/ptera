module Language.Parser.Ptera.Data.HFList (
    T,
    HFList (..),
    Membership,

    Concat,
    hconcat,

    hfoldrWithIndex,
    htraverseWithIndex,
    hmapWithIndex,
    hfoldMWithIndex,
    hforMWithIndex,
    hfoldlWithIndex,

    DictF (..),
) where

import           Language.Parser.Ptera.Prelude

import           Type.Membership               (Membership)
import qualified Unsafe.Coerce                 as Unsafe


type T = HFList

data HFList :: (k -> Type) -> [k] -> Type where
    HFNil :: HFList f '[]
    HFCons :: f x -> HFList f xs -> HFList f (x ': xs)


type family Concat (xs1 :: [k]) (xs2 :: [k]) :: [k] where
    Concat '[] xs2 =
        xs2
    Concat (x ': xs1) xs2 =
        x ': Concat xs1 xs2

hconcat :: HFList f xs1 -> HFList f xs2 -> HFList f (Concat xs1 xs2)
hconcat l1 l2 = case l1 of
    HFNil ->
        l2
    HFCons x l1' ->
        HFCons x do hconcat l1' l2

hfoldrWithIndex
    :: forall f r xs
    .  (forall x ys. Membership xs x -> f x -> r ys -> r (x ': ys)) -> r '[]
    -> HFList f xs -> r xs
hfoldrWithIndex f z0 = go 0 where
    go :: Int -> HFList f ys -> r ys
    go m0 = \case
        HFNil ->
            z0
        HFCons y l -> do
            let m1 = m0 + 1
            f
                do unsafeMembership m0
                do y
                do go m1 l

htraverseWithIndex
    :: forall m f g xs
    .  Applicative m
    => (forall x. Membership xs x -> f x -> m (g x))
    -> HFList f xs -> m (HFList g xs)
htraverseWithIndex f = coerce go0 where
    go0 = hfoldrWithIndex go do TraverseHFList do pure HFNil

    go
        :: forall x ys. Membership xs x -> f x
        -> TraverseHFList m g ys -> TraverseHFList m g (x ': ys)
    go m x = \case
        TraverseHFList acc0 ->
            TraverseHFList do HFCons <$> f m x <*> acc0

newtype TraverseHFList m f xs = TraverseHFList (m (HFList f xs))

hmapWithIndex
    :: (forall x. Membership xs x -> f x -> g x)
    -> HFList f xs -> HFList g xs
hmapWithIndex f l = runIdentity
    do htraverseWithIndex
        do \m x -> Identity do f m x
        do l

hfoldMWithIndex :: forall m r f xs
    .  Monad m
    => r -> (forall x. r -> Membership xs x -> f x -> m r)
    -> HFList f xs -> m r
hfoldMWithIndex z0 f = go 0 z0 where
    go :: Int -> r -> HFList f ys -> m r
    go m1 z1 = \case
        HFNil ->
            pure z1
        HFCons y l -> do
            z2 <- f z1
                do unsafeMembership m1
                do y
            let m2 = m1 + 1
            go m2 z2 l

hforMWithIndex
    :: forall m f xs
    .  Applicative m
    => HFList f xs -> (forall x. Membership xs x -> f x -> m ()) -> m ()
hforMWithIndex l0 f = go 0 l0 where
    go :: Int -> HFList f ys -> m ()
    go m1 = \case
        HFNil ->
            pure ()
        HFCons y l -> const
            <$> f
                do unsafeMembership m1
                do y
            <*> go
                do m1 + 1
                l

hfoldlWithIndex
    :: r -> (forall x. r -> Membership xs x -> f x -> r)
    -> HFList f xs -> r
hfoldlWithIndex z0 f = coerce
    do hfoldMWithIndex z0 \z m x -> Identity do f z m x


data DictF :: (k -> Constraint) -> k -> Type where
    DictF :: c x => DictF c x


unsafeMembership :: Int -> Membership xs x
unsafeMembership = Unsafe.unsafeCoerce
