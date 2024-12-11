module Eleven.PlutonianPebbles where

import Control.Parallel.Strategies (parMap, rpar)
import Data.Bifunctor (second)
import Data.Map (Map)
import Data.Map qualified as Map

---

main :: IO ()
main = do
  let inp = [5, 62914, 65, 972, 0, 805922, 6521, 1639064]
  print $ partOne inp
  print $ partTwo inp

-- Part 1

partOne :: [Int] -> Int
partOne stones = sum $ parMap rpar (fst . blinkNTimes 25 1 Map.empty) stones

-- Part 2

partTwo :: [Int] -> Int
partTwo stones = sum $ parMap rpar (fst . blinkNTimes 75 1 Map.empty) stones

-- Common

type Cache = Map (Int, Int, Int) Int -- (stone, blinks, counter) -> result

blinkNTimes :: Int -> Int -> Cache -> Int -> (Int, Cache)
blinkNTimes n counter cache stone =
  case Map.lookup (stone, n, counter) cache of
    Just cachedResult -> (cachedResult, cache)
    Nothing -> second (Map.insert (stone, n, counter) (fst result)) result
  where
    result
      | n == 0 = (counter, cache)
      | stone == 0 = blinkNTimes (n - 1) counter cache 1
      | even (length $ show stone) =
          let (left, right) = splitNumber stone
              (leftCount, leftCache) = blinkNTimes (n - 1) (counter + 1) cache left
              (rightCount, rightCache) = blinkNTimes (n - 1) 0 leftCache right
           in (leftCount + rightCount, rightCache)
      | otherwise = blinkNTimes (n - 1) counter cache (stone * 2024)

splitNumber :: Int -> (Int, Int)
splitNumber x =
  let s = show x
      len = length s `div` 2
      (l, r) = splitAt len s
   in (read l, read r)
