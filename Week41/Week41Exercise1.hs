module Week41Exercise1 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Graph n = Map n (Set n)

disjoint :: (Ord a) => Set a -> Set a -> Bool
disjoint s1 s2 = Set.null (Set.intersection s1 s2)

hasCycle :: (Ord n) => Graph n -> n -> Bool
hasCycle g startNode =
  case Map.lookup startNode g of
    Nothing -> False
    Just _ -> hasCycleHelper g startNode (Set.singleton startNode)

hasCycleHelper :: (Ord n) => Graph n -> n -> Set n -> Bool
hasCycleHelper g currentNode visitedNodes =
  case Map.lookup currentNode g of
    Nothing -> False
    Just children ->
      if not (disjoint children visitedNodes)
      then True
      else
        any (\child -> hasCycleHelper g child (Set.insert child visitedNodes)) (Set.toList children)