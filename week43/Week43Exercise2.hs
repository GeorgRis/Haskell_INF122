{-# LANGUAGE FlexibleInstances #-}

module Week43Exercise2 where

import Data.List (union, nub)
import qualified Data.Map as M

-- Datatypen MyGraph representeres som en Map fra en node (Integer) til en liste av naboer (Integer).
-- Vi bruker et typesynonym for enkelhets skyld.
type AdjMap = M.Map Integer [Integer]
newtype MyGraph = MyGraph AdjMap
  deriving (Show)

class IntegerGraph g where
  emptyGraph :: g
  insertNode :: Integer -> g -> g
  insertEdge :: Integer -> Integer -> g -> g
  nodeInGraph :: Integer -> g -> Bool
  edgeInGraph :: Integer -> Integer -> g -> Bool

-- a) Lag en instans av IntegerGraph for MyGraph
instance IntegerGraph MyGraph where
  -- Den tomme grafen er en tom Map
  emptyGraph = MyGraph M.empty

  -- Setter inn en node. Hvis den allerede finnes, gjøres ingenting.
  -- Hvis den er ny, legges den til med en tom liste av naboer.
  insertNode n (MyGraph adj) =
    MyGraph $ M.insertWith (const id) n [] adj

  -- Setter inn en kant fra 'from' til 'to'.
  -- Noder som ikke finnes, må legges til eksplisitt (dette er et krav i oppgaven).
  insertEdge from to graph =
    let
      -- Sørger for at begge nodene finnes i grafen først
      graph' = insertNode from (insertNode to graph)
      (MyGraph adj) = graph'
      -- Oppdaterer nabolisten til 'from' ved å legge til 'to' (unikt)
      -- M.insertWith bruker den gitte funksjonen (union . (:)) hvis nøkkelen ('from') finnes.
      -- union . (:) sørger for at 'to' legges til hvis den ikke allerede er der, uten duplikater.
      -- Hvis nøkkelen ikke finnes, brukes den nye verdien ([to]).
      newAdj = M.insertWith (union . (:)) from [to] adj
    in MyGraph newAdj

  -- Sjekker om en node finnes ved å se om den er en nøkkel i Map-en.
  nodeInGraph n (MyGraph adj) = M.member n adj

  -- Sjekker om det finnes en kant fra 'from' til 'to'.
  -- Returnerer False hvis 'from' ikke finnes.
  edgeInGraph from to (MyGraph adj) =
    case M.lookup from adj of
      Nothing -> False
      Just neighbours -> to `elem` neighbours

-- b) Lag graph :: (IntegerGraph g) => g
-- Grafen basert på bildet: Noder: {1, 3, 5, 6, 8}. Kanter: (5,1), (1,6), (1,8), (5,8), (8,5).
-- Noden 3 er isolert.

graph :: (IntegerGraph g) => g
graph =
  let
    -- Start med tom graf og sett inn alle noder
    g0 = emptyGraph
    g1 = foldr insertNode g0 [1, 3, 5, 6, 8]
    -- Sett inn alle kanter
    g2 = insertEdge 5 1 g1
    g3 = insertEdge 1 6 g2
    g4 = insertEdge 1 8 g3
    g5 = insertEdge 5 8 g4
    g6 = insertEdge 8 5 g5
  in g6

