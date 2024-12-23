module Twentythree.LANParty where

import Control.Monad (guard)
import Data.List (intercalate, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

---

main :: IO ()
main = do
  inp <- input
  print $ partOne inp
  print $ partTwo inp

-- Part 1

partOne :: [String] -> Int
partOne = findTrianglesWithT . buildGraph

findTrianglesWithT :: Graph -> Int
findTrianglesWithT graph = length $ do
  (a, b, c) <- orderedTriples
  guard $ any (`startsWith` 't') [a, b, c]
  pure [a, b, c]
  where
    orderedTriples = do
      a <- vertices
      b <- higherNeighbors a
      c <- higherNeighbors b
      guard $ connected a c
      pure (a, b, c)
    vertices = Map.keys graph
    neighbors x = Map.findWithDefault Set.empty x graph
    higherNeighbors x = filter (> x) $ Set.toList $ neighbors x
    connected x y = y `Set.member` neighbors x
    startsWith s c = head s == c

-- Part 2

partTwo :: [String] -> String
partTwo = findMaxClique . buildGraph

isClique :: Graph -> Set String -> Bool
isClique graph nodes = all allNeighborsPresent $ Set.toList nodes
  where
    allNeighborsPresent node =
      let neighbors = Map.findWithDefault Set.empty node graph
       in Set.null $ Set.difference (Set.delete node nodes) neighbors

findMaxClique :: Graph -> String
findMaxClique graph = intercalate "," $ snd $ maximum cliques
  where
    cliques = do
      (a, b) <- orderedPairs $ Map.keys graph
      let shared = intersection a b
      let potential = Set.fromList [a, b] `Set.union` shared
      guard $ isClique graph potential
      pure (Set.size potential, sort $ Set.toList potential)
    orderedPairs xs = [(x, y) | x <- xs, y <- neighbors x, x < y]
    neighbors x = Set.toList $ Map.findWithDefault Set.empty x graph
    intersection x y =
      Set.intersection
        (Map.findWithDefault Set.empty x graph)
        (Map.findWithDefault Set.empty y graph)

-- Common

type Graph = Map String (Set String)

parseEdge :: String -> (String, String)
parseEdge s = case break (== '-') s of
  (a, '-' : b) -> (a, b)
  _else -> error "invalid edge format"

buildGraph :: [String] -> Graph
buildGraph edges = Map.fromListWith Set.union [(v, Set.singleton u) | e <- edges, (v, u) <- toPairs (parseEdge e)]
  where
    toPairs (v, u) = [(v, u), (u, v)]

input :: IO [String]
input = lines <$> readFile "Twentythree/input.txt"
