module Ten.HoofIt where

import Data.Array.IArray
  ( Array,
    IArray (bounds),
    Ix (inRange, range),
    array,
    (!),
  )
import Data.Set qualified as Set

---

main :: IO ()
main = do
  inp <- input
  print $ partOne inp
  print $ partTwo inp

-- Part 1

partOne :: Grid -> Int
partOne grid =
  let removeDuplicates = Set.toList . Set.fromList
      scoreTrail = length . filter ((== 9) . (grid !)) . removeDuplicates . bfs grid
      scoredTrails = map scoreTrail (trailheads grid)
   in sum scoredTrails

-- Part 2

partTwo :: Grid -> Int
partTwo grid =
  let scoreTrail = length . filter ((== 9) . (grid !)) . bfs grid
      scoredTrails = map scoreTrail (trailheads grid)
   in sum scoredTrails

-- Common

bfs :: Grid -> Position -> [Position]
bfs grid start = go [start] []
  where
    go frontier seen | null frontier = seen
    go frontier seen = go newFront (seen ++ frontier)
      where
        newFront =
          [ n
            | p <- frontier,
              n <- neighbors p,
              grid ! n == (grid ! p) + 1
          ]
    neighbors (x, y) = filter (inRange $ bounds grid) directions
      where
        directions = [(x + 1, y), (x - 1, y), (x, y - 1), (x, y + 1)]

trailheads :: Grid -> [Position]
trailheads grid = [pos | pos <- range (bounds grid), grid ! pos == 0]

input :: IO Grid
input = do
  rawInput <- readFile "Ten/input.txt"
  let ls = lines rawInput
      h = length ls
      w = length (head ls)
  return $
    array
      ((0, 0), (w - 1, h - 1))
      [((x, y), read @Int [(ls !! y) !! x]) | y <- [0 .. h - 1], x <- [0 .. w - 1]]

type Grid = Array (Int, Int) Int

type Position = (Int, Int)
