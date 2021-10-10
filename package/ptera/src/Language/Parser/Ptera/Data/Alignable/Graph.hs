module Language.Parser.Ptera.Data.Alignable.Graph (
    T,
    Graph,
    fromArray,
    liftGraphOp,
    edges,
) where

import           Language.Parser.Ptera.Prelude

import qualified Language.Parser.Ptera.Data.Alignable as Alignable
import qualified Language.Parser.Ptera.Data.Alignable.Array as AlignableArray
import qualified Data.Graph                  as DataGraph
import qualified Data.Array as Array


type T = Graph

newtype Graph n = Graph DataGraph.Graph

fromArray :: Alignable.T n => AlignableArray.Array n [n] -> Graph n
fromArray = coerce

liftGraphOp :: (DataGraph.Graph -> DataGraph.Graph) -> Graph n -> Graph n
liftGraphOp f = coerce f

edges :: Alignable.T n => Graph n -> n -> [n]
edges = coerce do (Array.!) @Int @[Int]
