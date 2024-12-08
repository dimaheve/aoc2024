module Eight.ResonantCollinearity where

import Control.Monad (guard)
import Data.List (tails)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set

---

main :: IO ()
main = do
  (inp, bound) <- input
  print $ partOne bound inp
  print $ partTwo bound inp

-- Part 1

partOne :: (Int, Int) -> Map Char [Position] -> Int
partOne bound = length . Set.fromList . concat . Map.elems . findAntinodes True bound

-- Part 2

partTwo :: (Int, Int) -> Map Char [Position] -> Int
partTwo bound = length . Set.fromList . concat . Map.elems . findAntinodes False bound

-- Common

findAntinodes :: Bool -> (Int, Int) -> Map Char [Position] -> Map Char [Position]
findAntinodes solvePart1 bound = Map.map (concatMap getAntinodesOfPair . pairs)
  where
    getAntinodesOfPair ((x1, y1), (x2, y2)) =
      let (dx, dy) = (x2 - x1, y2 - y1)
          divisor = gcd dx dy
          (slopeX, slopeY) = (dx `div` divisor, dy `div` divisor)
          antinodes =
            if solvePart1
              then [(x1 + dx * 2, y1 + dy * 2), (x2 - dx * 2, y2 - dy * 2)]
              else [(x1 + slopeX * n, y1 + slopeY * n) | n <- [-(uncurry max bound) .. (uncurry max bound)]]
       in filter (inBounds bound) antinodes
    pairs positions = [(p1, p2) | (p1 : ps) <- tails positions, p2 <- ps]
    inBounds (maxX, maxY) (x, y) = x >= 0 && x <= maxX && y >= 0 && y <= maxY

type Position = (Int, Int)

input :: IO (Map Char [Position], (Int, Int))
input = do
  ls <- reverse . lines <$> readFile "Eight/input.txt"
  let h = length ls
      w = length (head ls)
      positions = do
        (y, row) <- zip [0 .. h - 1] ls
        (x, char) <- zip [0 .. w - 1] row
        guard (char /= '.')
        return (char, (x, y))
  return (Map.fromListWith (++) [(c, [p]) | (c, p) <- positions], (w - 1, h - 1))
