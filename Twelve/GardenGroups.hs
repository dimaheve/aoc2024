module Twelve.GardenGroups where

import Data.Array.IArray
  ( Array,
    IArray (bounds),
    Ix (inRange),
    array,
    indices,
    (!),
  )
import Data.Foldable (Foldable (toList))
import Data.Set (Set)
import Data.Set qualified as Set

---

main :: IO ()
main = do
  inp <- input
  print $ partOne inp
  print $ partTwo inp

-- Part 1

partOne :: Field -> Int
partOne field =
  let regions = findRegions field
      computePrice region = length region * getPerimeter region
      prices = map computePrice regions
   in sum prices

getPerimeter :: Set Coord -> Int
getPerimeter region = sum $ map (\coord -> (-) 4 (length $ neighborsInRegion coord region)) (toList region)

-- Part 2

partTwo :: Field -> Int
partTwo field =
  let regions = findRegions field
      computePrice region = length region * getRegionSides region
      prices = map computePrice regions
   in sum prices

getRegionSides :: Set Coord -> Int
getRegionSides region = sum $ map corners (toList region)
  where
    corners p = case length $ filter (`elem` region) $ directions p of
      0 -> 4
      1 -> 2
      _ -> sum [checkL p l | l <- lShapes]
    checkL (x, y) (n1, n2, i, o1, o2) =
      if all (`elem` region) [add (x, y) n1, add (x, y) n2]
        then cornerVal ((`notElem` region) $ add (x, y) i) (all (`notElem` region) [add (x, y) o1, add (x, y) o2])
        else 0
    cornerVal i o = case (i, o) of
      (True, True) -> 2
      (True, False) -> 1
      (False, True) -> 1
      _otherwise -> 0
    add (x, y) (dx, dy) = (x + dx, y + dy)
    lShapes =
      [ ((0, -1), (1, 0), (1, -1), (0, 1), (-1, 0)), -- top-left
        ((0, 1), (1, 0), (1, 1), (0, -1), (-1, 0)), -- bottom-left
        ((-1, 0), (0, -1), (-1, -1), (0, 1), (1, 0)), -- top-right
        ((-1, 0), (0, 1), (-1, 1), (0, -1), (1, 0)) -- bottom-right
      ]

-- Common

findRegions :: Field -> [Set Coord]
findRegions field =
  let go unvisited regions = case unvisited of
        [] -> regions
        (c : cs)
          | any (c `Set.member`) regions -> go cs regions
          | otherwise -> go cs (floodFill c : regions)
      floodFill start = go' (Set.singleton start) [start]
        where
          target = field ! start
          go' seen [] = seen
          go' seen (c : q) =
            let ns = [n | n <- directions c, inRange (bounds field) n, field ! n == target, not (n `Set.member` seen)]
                seen' = foldr Set.insert seen ns
             in go' seen' (q ++ ns)
   in go (indices field) []

neighborsInRegion :: Coord -> Set Coord -> [Coord]
neighborsInRegion (x, y) region = filter (`elem` region) $ directions (x, y)

directions :: Coord -> [Coord]
directions (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

type Field = Array (Int, Int) Char

type Coord = (Int, Int)

input :: IO Field
input = do
  rawInput <- readFile "Twelve/input.txt"
  let ls = lines rawInput
      h = length ls
      w = length (head ls)
  return $
    array
      ((0, 0), (w - 1, h - 1))
      [((x, y), (ls !! y) !! x) | y <- [0 .. h - 1], x <- [0 .. w - 1]]
