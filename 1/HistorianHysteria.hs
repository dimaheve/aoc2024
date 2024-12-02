module HistorianHysteria where

import Data.List (sortBy)
import Data.Ord (comparing)

-- Common

inputs = lines <$> readFile "inputs.txt"

removeSpace :: String -> (String, String)
removeSpace str =
  let (firstPart, rest) = break (== ' ') str
      secondPart = filter (/= ' ') (dropWhile (== ' ') rest)
   in (firstPart, secondPart)

removeSpaces :: [String] -> ([String], [String])
removeSpaces lst = removeSpaces' lst [] []
  where
    removeSpaces' [] a b = (a, b)
    removeSpaces' (x : xs) a b =
      let (c, d) = removeSpace x
       in removeSpaces' xs (c : a) (d : b)

-- Part 1

sortByDescending :: (Ord a) => [a] -> [a]
sortByDescending = sortBy (flip compare)

partOne :: [String] -> Int
partOne inp = 
  let (a, b) = removeSpaces inp
      aOfInts = map (read @Int) a
      bOfInts = map (read @Int) b
      aSorted = sortByDescending aOfInts
      bSorted = sortByDescending bOfInts
      diff = zipWith (\a b -> if a >= b then a - b else b - a) aSorted bSorted
  in sum diff

-- Part 2

countRepeated :: Int -> [Int] -> Int
countRepeated x = length . filter (== x)

partTwo :: [String] -> Int
partTwo inp = 
  let (a, b) = removeSpaces inp
      aOfInts = map (read @Int) a
      bOfInts = map (read @Int) b
      repeats = map (`countRepeated` bOfInts) aOfInts
      mulRepeats = zipWith (*) aOfInts repeats
  in sum mulRepeats

---

main :: IO ()
main = do
  inp <- inputs
  print $ partOne inp
  print $ partTwo inp
