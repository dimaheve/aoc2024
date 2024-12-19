module Nineteen.LinenLayout where

import Data.List (isPrefixOf)
import Data.Vector.Unboxed qualified as V
import Control.Parallel.Strategies (rpar, parMap)

---

main :: IO ()
main = do
  (towels, designs) <- input
  let waysToConstruct = parMap rpar (countWaysToConstruct towels) designs
  -- Part 1
  print $ length . filter (>= 1) $ waysToConstruct
  -- Part 2
  print $ sum waysToConstruct

-- Common

countWaysToConstruct :: [Pattern] -> Pattern -> Int
countWaysToConstruct towels design = go 0 initialWays
  where
    initialWays = V.singleton 1 V.++ V.replicate len 0
    len = length design
    go i ways
      | i == len = ways V.! len
      | ways V.! i == 0 = go (i + 1) ways
      | otherwise = go (i + 1) $ foldr (tryPattern i) ways towels
    tryPattern i pat ways
      | pat `isPrefixOf` drop i design = ways V.// [(newPos, ways V.! i + ways V.! newPos)]
      | otherwise = ways
      where
        newPos = i + length pat

data Color = White | Blue | Black | Red | Green deriving (Show, Ord, Eq)

type Pattern = [Color]

input :: IO ([Pattern], [Pattern])
input = do
  (towels, designs) <- break null . lines <$> readFile "Nineteen/input.txt"
  let parsedTowels = map (map parseRGB . takeWhile (/= ',')) $ words $ concat towels
      parsedDesigns = map (map parseRGB) $ drop 1 designs
  return (parsedTowels, parsedDesigns)
  where
    parseRGB ch = case ch of
      'w' -> White
      'u' -> Blue
      'b' -> Black
      'r' -> Red
      'g' -> Green
      _invalid -> error "invalid color character in input"
