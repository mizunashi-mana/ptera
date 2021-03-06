module Language.Parser.Ptera.Data.Symbolic.IntMap where

import           Language.Parser.Ptera.Prelude

import qualified Data.HashMap.Strict                        as HashMap
import qualified Data.IntMap.Strict                         as DataIntMap
import qualified Data.IntSet                                as DataIntSet
import qualified Language.Parser.Ptera.Data.Symbolic.IntSet as IntSet


type T = IntMap

type Key = Int

data IntMap a = IntMap
    {
        intMapStraight :: DataIntMap.IntMap (Maybe a),
        intMapNegative :: Maybe a
    }
    deriving (Show, Functor, Foldable, Traversable)

empty :: IntMap a
empty = IntMap
    {
        intMapStraight = DataIntMap.empty,
        intMapNegative = Nothing
    }

full :: a -> IntMap a
full v = IntMap
    {
        intMapStraight = DataIntMap.empty,
        intMapNegative = Just v
    }

singleton :: Key -> a -> IntMap a
singleton k v = IntMap
    {
        intMapStraight = DataIntMap.singleton k do Just v,
        intMapNegative = Nothing
    }

normalize :: Eq a => IntMap a -> IntMap a
normalize m = case intMapNegative m of
    Nothing -> m
        {
            intMapStraight = DataIntMap.mapMaybe
                do \x -> Just <$> x
                do intMapStraight m
        }
    Just nx -> m
        {
            intMapStraight = DataIntMap.mapMaybe
                do \case
                    Nothing ->
                        Just Nothing
                    Just x | x == nx ->
                        Nothing
                    jx@Just{} ->
                        Just jx
                do intMapStraight m
        }

instance Eq a => Eq (IntMap a) where
    m1 == m2 = intMapNegative m1 == intMapNegative m2
        && intMapStraight (normalize m1) == intMapStraight (normalize m2)

insert :: Key -> a -> IntMap a -> IntMap a
insert k v m = m
    {
        intMapStraight = DataIntMap.insert k
            do Just v
            do intMapStraight m
    }

insertBulk :: IntSet.T -> a -> IntMap a -> IntMap a
insertBulk ss v m0 = case ss of
    IntSet.StraightSet s -> do
        let jv = Just v
        IntMap
            { intMapStraight = foldl'
                do \m k -> DataIntMap.insert k jv m
                do intMapStraight m0
                do DataIntSet.elems s
            , intMapNegative = intMapNegative m0
            }
    IntSet.NegativeSet s -> IntMap
        { intMapStraight = DataIntMap.restrictKeys
            do intMapStraight m0
            do s
        , intMapNegative = Just v
        }

delete :: Key -> IntMap a -> IntMap a
delete k m = case intMapNegative m of
    Nothing -> m
        {
            intMapStraight = DataIntMap.delete k do intMapStraight m
        }
    Just _ -> m
        {
            intMapStraight = DataIntMap.insert k Nothing do intMapStraight m
        }

update :: (a -> Maybe a) -> Key -> IntMap a -> IntMap a
update f k m = case DataIntMap.lookup k do intMapStraight m of
    Just mv -> go mv
    Nothing -> go do intMapNegative m
    where
        go = \case
            Nothing -> m
            Just v -> case f v of
                Nothing -> m
                jv@Just{} -> m
                    {
                        intMapStraight = DataIntMap.insert k jv do intMapStraight m
                    }

alter :: (Maybe a -> Maybe a) -> Key -> IntMap a -> IntMap a
alter f k m = case DataIntMap.lookup k do intMapStraight m of
    Just mv -> go mv
    Nothing -> go do intMapNegative m
    where
        go = \case
            Nothing -> case f Nothing of
                Nothing -> m
                jv@Just{} -> m
                    {
                        intMapStraight = DataIntMap.insert k jv do intMapStraight m
                    }
            jv0@Just{} -> m
                {
                    intMapStraight = DataIntMap.insert k
                        do f jv0
                        do intMapStraight m
                }

alterBulk :: (Maybe a -> Maybe a) -> IntSet.T -> IntMap a -> IntMap a
alterBulk f ks m0 = case ks of
    IntSet.StraightSet s -> case intMapNegative m0 of
        Nothing -> m0
            {
                intMapStraight = foldl'
                    do \m k -> DataIntMap.alter
                        do \mmv -> case f do join mmv of
                            Nothing   -> Nothing
                            jv@Just{} -> Just jv
                        k m
                    do intMapStraight m0
                    do DataIntSet.elems s
            }
        njv@Just{} -> m0
            {
                intMapStraight = foldl'
                    do \m k -> DataIntMap.alter
                        do \case
                            Nothing -> Just do f njv
                            Just mv -> Just do f mv
                        k m
                    do intMapStraight m0
                    do DataIntSet.elems s
            }
    IntSet.NegativeSet s -> case intMapNegative m0 of
        Nothing -> case f Nothing of
            Nothing -> m0
                {
                    intMapStraight = DataIntMap.mapMaybeWithKey
                        do \k mv0 -> do
                            _ <- mv0
                            if DataIntSet.member k s
                                then pure mv0
                                else Just <$> f mv0
                        do intMapStraight m0
                }
            jv@Just{} -> IntMap
                { intMapStraight = DataIntMap.mapMaybeWithKey
                    do \k mv0 -> if DataIntSet.member k s
                        then pure mv0
                        else case mv0 of
                            Nothing -> pure jv
                            Just{}  -> pure do f mv0
                    do intMapStraight m0
                , intMapNegative = jv
                }
        njv0@Just{} -> case f njv0 of
            Nothing -> IntMap
                { intMapStraight = DataIntMap.mapMaybeWithKey
                    do \k mv0 -> if DataIntSet.member k s
                        then Just <$> mv0
                        else Just <$> f mv0
                    do intMapStraight m0
                , intMapNegative = Nothing
                }
            njv@Just{} -> IntMap
                { intMapStraight = DataIntMap.mapMaybeWithKey
                    do \k mv0 -> if DataIntSet.member k s
                        then pure mv0
                        else pure do f mv0
                    do intMapStraight m0
                , intMapNegative = njv
                }

lookup :: Key -> IntMap a -> Maybe a
lookup k m = case DataIntMap.lookup k do intMapStraight m of
    Just mv -> mv
    Nothing -> intMapNegative m

keys :: IntMap a -> IntSet.T
keys m = case intMapNegative m of
    Just{}  -> IntSet.NegativeSet
        do DataIntSet.fromList
            [ k
            | (k, mv) <- DataIntMap.assocs do intMapStraight m
            , case mv of { Nothing -> True; Just{} -> False }
            ]
    Nothing -> IntSet.StraightSet
        do DataIntSet.fromList
            [ k
            | (k, mv) <- DataIntMap.assocs do intMapStraight m
            , case mv of { Nothing -> False; Just{} -> True }
            ]

restrictKeys :: IntMap a -> IntSet.T -> IntMap a
restrictKeys m s = case intMapNegative m of
    Nothing -> case s of
        IntSet.StraightSet is ->
            IntMap
                { intMapNegative = Nothing
                , intMapStraight = DataIntMap.restrictKeys
                    do intMapStraight m
                    do is
                }
        IntSet.NegativeSet is ->
            IntMap
                { intMapNegative = Nothing
                , intMapStraight = DataIntMap.withoutKeys
                    do intMapStraight m
                    do is
                }
    notMx@Just{} -> case s of
        IntSet.StraightSet is -> do
            let notM = DataIntMap.fromSet
                    do \_ -> notMx
                    do is
            IntMap
                { intMapNegative = Nothing
                , intMapStraight = DataIntMap.unionWith
                    do \x _ -> x
                    do intMapStraight m
                    do notM
                }
        IntSet.NegativeSet is -> do
            let deleteM = DataIntMap.fromSet
                    do \_ -> Nothing
                    do is
            IntMap
                { intMapNegative = notMx
                , intMapStraight = DataIntMap.unionWith
                    do \_ x -> x
                    do intMapStraight m
                    do deleteM
                }



merge :: (a -> b -> Maybe c) -> (a -> Maybe c) -> (b -> Maybe c) -> IntMap a -> IntMap b -> IntMap c
merge fab fa fb = \sma0 smb0 -> case intMapNegative sma0 of
    Nothing -> case intMapNegative smb0 of
        Nothing -> goMergeStraight sma0 smb0
        Just nb0 -> case fb nb0 of
            Nothing  -> goMergeStraight sma0 smb0
            Just nb1 -> goMergeNegative nb1 sma0 smb0
    Just na0 -> case intMapNegative smb0 of
        Nothing -> case fa na0 of
            Nothing  -> goMergeStraight sma0 smb0
            Just na1 -> goMergeNegative na1 sma0 smb0
        Just nb0 -> case fab na0 nb0 of
            Nothing   -> goMergeStraight sma0 smb0
            Just nab1 -> goMergeNegative nab1 sma0 smb0
    where
        goMergeStraight sma0 smb0 = IntMap
            { intMapStraight = DataIntMap.mergeWithKey
                do \_ mx my -> case mx of
                    Nothing -> case my of
                        Nothing -> Nothing
                        Just y  -> Just <$> fb y
                    Just x  -> case my of
                        Nothing -> Just <$> fa x
                        Just y  -> Just <$> fab x y
                do \ma -> case intMapNegative smb0 of
                    Nothing -> DataIntMap.mapMaybe
                        do \mx -> fmap Just do mx >>= fa
                        do ma
                    Just nb1 -> DataIntMap.mapMaybe
                        do \mx -> fmap Just do mx >>= \x -> fab x nb1
                        do ma
                do \mb -> case intMapNegative sma0 of
                    Nothing -> DataIntMap.mapMaybe
                        do \my -> fmap Just do my >>= fb
                        do mb
                    Just na1 -> DataIntMap.mapMaybe
                        do \my -> fmap Just do my >>= \y -> fab na1 y
                        do mb
                do intMapStraight sma0
                do intMapStraight smb0
            , intMapNegative = Nothing
            }

        goMergeNegative n1 sma0 smb0 = IntMap
            { intMapStraight = DataIntMap.mergeWithKey
                do \_ mx my -> case mx of
                    Nothing -> case my of
                        Nothing -> Just Nothing
                        Just y  -> Just do fb y
                    Just x  -> case my of
                        Nothing -> Just do fa x
                        Just y  -> Just do fab x y
                do \ma -> case intMapNegative smb0 of
                    Nothing -> fmap
                        do \mx -> mx >>= fa
                        do ma
                    Just nb1 -> fmap
                        do \mx -> mx >>= \x -> fab x nb1
                        do ma
                do \mb -> case intMapNegative sma0 of
                    Nothing -> fmap
                        do \my -> my >>= fb
                        do mb
                    Just na1 -> fmap
                        do \my -> my >>= \y -> fab na1 y
                        do mb
                do intMapStraight sma0
                do intMapStraight smb0
            , intMapNegative = Just n1
            }

groupBy :: Eq b => Hashable b => (a -> b) -> IntMap a -> HashMap.HashMap b IntSet.T
groupBy f m0 = case intMapNegative m0 of
    Nothing -> foldl'
        do \m (k, mv) -> case mv of
            Nothing ->
                m
            Just v -> do
                let fv = f v
                HashMap.alter
                    do \case
                        Just ks -> Just do IntSet.insert k ks
                        Nothing -> Just do IntSet.singleton k
                    fv m
        do HashMap.empty
        do DataIntMap.assocs do intMapStraight m0
    Just nv -> do
        let fnv = f nv
        let (m1, nks1) = foldl'
                do \(m, nks) (k, mv) -> case mv of
                    Nothing ->
                        (m, IntSet.delete k nks)
                    Just v -> do
                        let fv = f v
                        if fv == fnv
                            then (m, nks)
                            else
                                ( HashMap.alter
                                    do \case
                                        Just ks -> Just do IntSet.insert k ks
                                        Nothing -> Just do IntSet.singleton k
                                    fv m
                                , nks
                                )
                do (HashMap.empty, IntSet.full)
                do DataIntMap.assocs do intMapStraight m0
        HashMap.insert fnv nks1 m1
