{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module Six.GuardGallivant where

import Data.Array (Array, Ix (inRange), array, assocs, bounds, (!))
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Hashable (Hashable)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics (Generic)

-- Common

type Grid = Array (Int, Int) Char

type Coordinate = (Int, Int)

type State = (Coordinate, Direction)

data Direction = North | East | South | West
  deriving (Eq, Ord, Show, Generic, Hashable)

input :: IO Grid
input = do
  ls <- fmap (reverse . lines) (readFile "Six/input.txt")
  let h = length ls
      w = length (head ls)
  return $
    array
      ((0, 0), (w - 1, h - 1))
      [((x, y), (ls !! y) !! x) | y <- [0 .. h - 1], x <- [0 .. w - 1]]

inBounds :: Grid -> Coordinate -> Bool
inBounds g = inRange (bounds g)

charAt :: Grid -> Coordinate -> Maybe Char
charAt g c = if inBounds g c then Just (g ! c) else Nothing

move :: Coordinate -> Direction -> Coordinate
move (x, y) = \case
  North -> (x, y + 1)
  East -> (x + 1, y)
  South -> (x, y - 1)
  West -> (x - 1, y)

turnRight :: Direction -> Direction
turnRight = \case
  North -> East
  East -> South
  South -> West
  West -> North

findStart :: Grid -> Char -> Coordinate
findStart grid ch = head [p | (p, c) <- assocs grid, c == ch]

travelPath :: Grid -> Coordinate -> Direction -> Vector State
travelPath grid startPos startDir = V.fromList (go [(startPos, startDir)] startPos startDir)
  where
    go acc pos dir =
      let nxt = move pos dir
       in case charAt grid nxt of
            Just '#' -> go acc pos (turnRight dir)
            Just _ -> go ((nxt, dir) : acc) nxt dir
            Nothing -> acc

partOne :: Grid -> Int
partOne grid =
  let startPos = findStart grid '^'
      path = travelPath grid startPos North
   in HS.size . HS.fromList . map fst . V.toList $ path

-- Part Two

data Result = Loops | Escapes deriving (Eq, Show)

simulate :: Grid -> Coordinate -> Direction -> Coordinate -> Result
simulate grid startPos startDir obstacle = go startPos startDir HS.empty
  where
    go pos dir visited
      | not (inBounds grid pos) = Escapes
      | (pos, dir) `HS.member` visited = Loops
      | otherwise =
          let nxt = move pos dir
              visited' = HS.insert (pos, dir) visited
           in if nxt == obstacle
                then go pos (turnRight dir) visited'
                else case charAt grid nxt of
                  Just '#' -> go pos (turnRight dir) visited'
                  Just _ -> go nxt dir visited'
                  Nothing -> Escapes

possibleObstacles :: Vector State -> HashSet Coordinate
possibleObstacles path =
  let startPos = fst (V.last path)
   in HS.delete startPos . HS.fromList . map fst . V.toList $ path

partTwo :: Grid -> Int
partTwo grid =
  let startPos = findStart grid '^'
      originalPath = travelPath grid startPos North
      obstacles = possibleObstacles originalPath
   in HS.size $ HS.filter (\obstacle -> simulate grid startPos North obstacle == Loops) obstacles

---

main :: IO ()
main = do
  inp <- input
  print $ partOne inp
  print $ partTwo inp
