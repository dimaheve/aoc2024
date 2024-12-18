module Eighteen.RAMRun where

import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (ViewL (..))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set

---

main :: IO ()
main = do
  inp <- input
  print $ partOne inp
  print $ partTwo inp

-- Part 1

partOne :: [Coordinate] -> Int
partOne = fromMaybe (error "no path found") . flip bfs bounds . Set.fromList . take 1024
  where
    bounds = ((0, 0), (70, 70))

-- Part 2

partTwo :: [Coordinate] -> Coordinate
partTwo coords = coords !! (binarySearch 1024 (length coords) - 1)
  where
    bounds = ((0, 0), (70, 70))
    corrupted n = Set.fromList $ take n coords
    canReach n = isJust $ bfs (corrupted n) bounds
    binarySearch low high
      | low + 1 == high = high
      | otherwise =
          let mid = (low + high) `div` 2
           in if canReach mid
                then binarySearch mid high
                else binarySearch low mid

-- Common

bfs :: Set Coordinate -> Bounds -> Maybe Int
bfs corruptedCoordinates bounds = go (Seq.singleton ((0, 0), 0)) (Set.singleton (0, 0))
  where
    ((minX, minY), (maxX, maxY)) = bounds
    target = (maxX, maxY)
    go queue seen = case Seq.viewl queue of
      EmptyL -> Nothing
      ((pos@(x, y), steps) :< rest)
        | pos == target -> Just steps
        | otherwise ->
            let neighbors = filter (valid seen) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
                newQueue = rest <> Seq.fromList [(p, steps + 1) | p <- neighbors]
                newSeen = foldr Set.insert seen neighbors
             in go newQueue newSeen
    valid seen (x, y) = x >= minX && x <= maxX && y >= minY && y <= maxY && notCorrupted && notSeen
      where
        notCorrupted = not (Set.member (x, y) corruptedCoordinates)
        notSeen = not (Set.member (x, y) seen)

inRange :: Bounds -> Coordinate -> Bool
inRange ((minX, minY), (maxX, maxY)) (x, y) =
  x >= minX && x <= maxX && y >= minY && y <= maxY

input :: IO [Coordinate]
input = do
  content <- readFile "Eighteen/input.txt"
  return $ map ((\xs -> (read $ head xs, read $ last xs)) . words . map commaToSpace) $ lines content
  where
    commaToSpace ',' = ' '
    commaToSpace c = c

type Coordinate = (Int, Int)

type Bounds = ((Int, Int), (Int, Int))
