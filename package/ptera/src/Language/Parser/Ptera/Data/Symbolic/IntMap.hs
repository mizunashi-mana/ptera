module Language.Parser.Ptera.Data.Symbolic.IntMap where

import           Language.Parser.Ptera.Prelude

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
    deriving (Eq, Show, Functor)

empty :: IntMap a
empty = IntMap
    {
        intMapStraight = DataIntMap.empty,
        intMapNegative = Nothing
    }

singleton :: Key -> a -> IntMap a
singleton k v = IntMap
    {
        intMapStraight = DataIntMap.singleton k do Just v,
        intMapNegative = Nothing
    }

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
