module First where

import Data.List (sortBy)
import Data.Ord (comparing)

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

sortByDescending :: (Ord a) => [a] -> [a]
sortByDescending = sortBy (flip compare)

main :: IO ()
main = do
  inp <- inputs
  let (a, b) = removeSpaces inp
      aOfInts = map (read @Int) a
      bOfInts = map (read @Int) b
      aSorted = sortByDescending aOfInts
      bSorted = sortByDescending bOfInts
      diff = zipWith (\a b -> if a >= b then a - b else b - a) aSorted bSorted
      finalSum = sum diff

  print finalSum
