{-# LANGUAGE OverloadedRecordDot #-}

module Six.GuardGallivant where

import Control.Parallel.Strategies
import Data.Array.IArray
import Data.Bifunctor (Bifunctor (bimap))
import Data.List
import Data.Maybe
import Data.Set qualified as Set

-- Common

type Grid = Array (Int, Int) Bool

type Position = (Int, Int)

type Direction = (Int, Int)

data Guard where
  Guard :: {pos :: Position, dir :: Position} -> Guard
  deriving (Show, Eq)

input :: IO (Grid, Position)
input = do
  rawInput <- readFile "Six/input.txt"
  let ls = (reverse . lines) rawInput
      h = length ls
      w = length (head ls)
  return $
    ( array
        ((0, 0), (w - 1, h - 1))
        [((x, y), ((ls !! y) !! x) == '#') | y <- [0 .. h - 1], x <- [0 .. w - 1]],
      findStart rawInput
    )

findStart :: String -> Position
findStart text =
  head $
    [ (c, r)
      | c <- [0 .. maxC],
        r <- [0 .. maxR],
        rows !! r !! c == '^'
    ]
  where
    rows = reverse $ lines text
    maxR = length rows - 1
    maxC = length (head rows) - 1

turnRight :: Direction -> Direction
turnRight (x, y) = (y, -x)

step :: Grid -> Guard -> Maybe (Position, Guard)
step grid guard
  | not (inRange (bounds grid) guard.pos) = Nothing
  | not (inRange (bounds grid) ahead) = Just (guard.pos, guard {pos = ahead})
  | grid ! ahead = Just (guard.pos, guard {dir = turnRight guard.dir})
  | otherwise = Just (guard.pos, guard {pos = ahead})
  where
    ahead = bimap (fst guard.pos +) (snd guard.pos +) guard.dir

walk :: Grid -> Guard -> [Position]
walk grid = unfoldr (step grid)

-- Part 1

partOne :: Grid -> Position -> Int
partOne inp start = length . Set.fromList $ walk inp (Guard start (0, 1))

-- Part 2

isLoop :: Guard -> [Guard] -> Grid -> Bool
isLoop guard trail grid
  | isNothing stepped = False
  | hasTurned && guard `elem` trail = True
  | hasTurned = isLoop guard' (guard : trail) grid
  | otherwise = isLoop guard' trail grid
  where
    stepped = step grid guard
    (_, guard') = fromJust stepped
    hasTurned = guard.dir /= guard'.dir

partTwo :: Grid -> Position -> Int
partTwo grid start = length $ filter id loopResults
  where
    modifiedGrids =
      [ grid // [(new, True)]
        | new <- news,
          new /= guard.pos
      ]
    loopResults = parMap rpar (isLoop guard []) modifiedGrids
    news = Set.toList . Set.fromList $ walk grid guard
    guard = Guard start (0, 1)

---

main :: IO ()
main = do
  (inp, start) <- input
  print $ partOne inp start
  print $ partTwo inp start
