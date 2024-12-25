module Twentyfive.CodeChronicle where

import Data.Either (partitionEithers)
import Data.List (transpose)
import Data.List.Split (splitOn)

---

main :: IO ()
main = input >>= print . partOne

-- Part 1

partOne :: ([Lock], [Key]) -> Int
partOne (locks, keys) =
  let pairs = [(l, k) | l <- locks, k <- keys]
   in length $ filter (\(l, k) -> all (<= 7) $ zipWith (+) l k) pairs

-- Common

countHeight :: Bool -> String -> Int
countHeight isLock col =
  if isLock
    then length $ takeWhile (== '#') col
    else length $ takeWhile (== '#') $ reverse col

input :: IO ([Lock], [Key])
input = do
  content <- readFile "Twentyfive/input.txt"
  let schematics = filter (not . null) $ splitOn "\n\n" content
      parseAndClassify s =
        if '#' `elem` head (lines s)
          then Left $ parseSchematic s
          else Right $ parseSchematic s
  return $ partitionEithers $ map parseAndClassify schematics

parseSchematic :: String -> Lock
parseSchematic schematic =
  let rows = lines schematic
      cols = transpose rows
      isLock = '#' `elem` head rows
   in map (countHeight isLock) cols

type Lock = [Int]

type Key = [Int]
