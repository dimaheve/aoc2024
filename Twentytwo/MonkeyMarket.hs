module Twentytwo.MonkeyMarket where

import Data.Bits (Bits (shiftL, shiftR), xor)
import Data.List (zip4)
import Data.Map.Strict qualified as Map
import Control.Parallel.Strategies (rpar, parMap)

---

main :: IO ()
main = do
  inp <- input
  let secrets = parMap rpar (take 2000 . iterate generateNextSecret) inp
  print $ sum $ parMap rpar last secrets -- Part 1
  print $ partTwo secrets

-- Part 2

partTwo :: [[Int]] -> Int
partTwo =
  maximum
    . Map.elems
    . foldSequenceMaps
    . parMap rpar (makeSequence . getPricesAndDeltas)
  where
    foldSequenceMaps = foldr (Map.unionWith (+)) Map.empty

getPricesAndDeltas :: [Int] -> [(Int, Int)]
getPricesAndDeltas secrets =
  let prices = map (`mod` 10) secrets
      deltas = zipWith (-) (tail prices) prices
   in zip prices deltas

makeSequence :: [(Int, Int)] -> Map.Map (Int, Int, Int, Int) Int
makeSequence pricesAndDeltas =
  let (prices, deltas) = unzip pricesAndDeltas
      slidingWindow = zip (toDeltaQuads deltas) (drop 4 prices)
   in foldr insertFirstOccurrence Map.empty slidingWindow
  where
    toDeltaQuads ds = zip4 ds (drop 1 ds) (drop 2 ds) (drop 3 ds)
    insertFirstOccurrence (seq, price) = Map.insertWith const seq price

-- Common

generateNextSecret :: Int -> Int
generateNextSecret n =
  let n1 = (n `xor` (n `shiftL` 6)) `mod` modulus
      n2 = (n1 `xor` (n1 `shiftR` 5)) `mod` modulus
      n3 = (n2 `xor` (n2 `shiftL` 11)) `mod` modulus
   in n3

modulus :: Int
modulus = 16777216

input :: IO [Int]
input = do
  rawInput <- readFile "Twentytwo/input.txt"
  return $ map read $ lines rawInput
