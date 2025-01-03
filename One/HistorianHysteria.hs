module One.HistorianHysteria where

import Data.List (sortBy)

-- Common

inputs :: IO [(Int, Int)]
inputs = do 
  inp <- lines <$> readFile "One/inputs.txt"
  let spacedInput = map words inp
      parsedInput = map (\x -> (read @Int $ head x, read @Int $ last x)) spacedInput
  return parsedInput

-- Part 1

sortByDescending :: (Ord a) => [a] -> [a]
sortByDescending = sortBy (flip compare)

partOne :: [(Int, Int)] -> Int
partOne inp = 
  let a = map fst inp
      b = map snd inp
      aSorted = sortByDescending a
      bSorted = sortByDescending b
      diff = zipWith (\c d -> if c >= d then c - d else d - c) aSorted bSorted
  in sum diff

-- Part 2

countRepeated :: Int -> [Int] -> Int
countRepeated x = length . filter (== x)

partTwo :: [(Int, Int)] -> Int
partTwo inp = 
  let a = map fst inp
      b = map snd inp
      repeats = map (`countRepeated` b) a
      mulRepeats = zipWith (*) a repeats
  in sum mulRepeats

---

main :: IO ()
main = do
  inp <- inputs
  print $ partOne inp
  print $ partTwo inp
