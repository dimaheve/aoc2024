module Thirdteen.ClawContraption where

import Data.List.Split (chunksOf)

---

main :: IO ()
main = do
  inp <- input
  print $ partOne inp

-- Part 1

partOne :: [((Integer, Integer), (Integer, Integer), (Integer, Integer))] -> Integer
partOne = sum . map (maybe 0 (\(a, b) -> 3 * a + b) . solve False)

-- Part 2

partTwo :: [((Integer, Integer), (Integer, Integer), (Integer, Integer))] -> Integer
partTwo = sum . map (maybe 0 (\(a, b) -> 3 * a + b) . solve False)

-- Common

solve :: Bool -> ((Integer, Integer), (Integer, Integer), (Integer, Integer)) -> Maybe (Integer, Integer)
solve limitTo100 ((ax, ay), (bx, by), (tx, ty)) =
  let det = ax * by - ay * bx
      a = (tx * by - ty * bx) `div` det
      b = (ax * ty - ay * tx) `div` det
   in if det /= 0
        && a >= 0
        && (a <= 100 || limitTo100)
        && b >= 0
        && (b <= 100 || limitTo100)
        && ax * a + bx * b == tx
        && ay * a + by * b == ty
        then Just (a, b)
        else Nothing

input :: IO [((Integer, Integer), (Integer, Integer), (Integer, Integer))]
input = do
  rawInput <- chunksOf 3 . filter (/= "") . lines <$> readFile "Thirdteen/input.txt"
  return $ map parseGame rawInput
  where
    parseGame l =
      let a = drop 2 (words (l !! 0))
          ax = read $ takeWhile (/= ',') $ drop 2 (a !! 0)
          ay = read $ drop 2 (a !! 1)
          b = drop 2 (words (l !! 1))
          bx = read $ takeWhile (/= ',') $ drop 2 (b !! 0)
          by = read $ drop 2 (b !! 1)
          r = drop 1 (words (l !! 2))
          rx = read $ takeWhile (/= ',') $ drop 2 (r !! 0)
          ry = read $ drop 2 (r !! 1)
       in ((ax, ay), (bx, by), (rx, ry))
