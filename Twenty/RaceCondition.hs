module Twenty.RaceCondition where

import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set

---

main :: IO ()
main = do
  grid <- input
  print $ findCheats grid 2 100 True
  print $ findCheats grid 20 100 False

-- Common

input :: IO Grid
input = lines <$> readFile "Twenty/input.txt"

findStartEnd :: Grid -> (Pos, Pos)
findStartEnd grid =
  let positions =
        [ (x, y)
          | y <- [0 .. length grid - 1],
            x <- [0 .. length (head grid) - 1]
        ]
      start = head [pos | pos <- positions, gridAt grid pos == 'S']
      end = head [pos | pos <- positions, gridAt grid pos == 'E']
   in (start, end)

gridAt :: Grid -> Pos -> Char
gridAt grid (x, y)
  | y >= 0
      && y < length grid
      && x >= 0
      && x < length (head grid) =
      grid !! y !! x
  | otherwise = '#'

manhattan :: Pos -> Pos -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

buildDistanceMap :: Grid -> Pos -> DistMap
buildDistanceMap grid start = dijkstra initial (Set.singleton start)
  where
    initial = Map.singleton start 0
    dijkstra :: DistMap -> Set.Set Pos -> DistMap
    dijkstra dists seen
      | Set.null seen = dists
      | otherwise =
          let curr = Set.findMin seen
              dist = fromMaybe maxBound (Map.lookup curr dists)
              neighbors =
                [ (x + dx, y + dy)
                  | let (x, y) = curr,
                    (dx, dy) <- [(0, 1), (1, 0), (0, -1), (-1, 0)],
                    gridAt grid (x + dx, y + dy) /= '#'
                ]
              validNeighbors = filter (\p -> not $ Map.member p dists) neighbors
              newDists =
                foldr
                  (\p m -> Map.insert p (dist + 1) m)
                  dists
                  validNeighbors
              newSeen = foldr Set.insert (Set.delete curr seen) validNeighbors
           in dijkstra newDists newSeen

findCheats :: Grid -> Int -> Int -> Bool -> Int
findCheats grid maxSteps minSaving exactSteps = length validCheats
  where
    (start, end) = findStartEnd grid
    fromStart = buildDistanceMap grid start
    fromEnd = buildDistanceMap grid end
    basePathLength = fromMaybe maxBound (Map.lookup end fromStart)
    validPositions =
      [ (x, y)
        | y <- [0 .. length grid - 1],
          x <- [0 .. length (head grid) - 1],
          gridAt grid (x, y) /= '#'
      ]
    validCheats =
      [ (s, e)
        | s <- validPositions,
          e <- validPositions,
          let cheatingDist = manhattan s e,
          if exactSteps
            then cheatingDist == maxSteps
            else cheatingDist <= maxSteps,
          let totalDist =
                fromMaybe maxBound (Map.lookup s fromStart)
                  + cheatingDist
                  + fromMaybe maxBound (Map.lookup e fromEnd),
          basePathLength - totalDist >= minSaving
      ]

type Pos = (Int, Int)

type Grid = [[Char]]

type DistMap = Map.Map Pos Int
