module Week41Exercise0 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Graph n = Map n (Set n)
insertEdge :: (Ord n) => n -> n -> Graph n -> Graph n
insertEdge n1 n2 g =
  let
    g' = Map.insertWith Set.union n1 (Set.singleton n2) g
    ensureNodeExists node graph = Map.insertWith (\_ old -> old) node Set.empty graph
  in
    ensureNodeExists n2 g'