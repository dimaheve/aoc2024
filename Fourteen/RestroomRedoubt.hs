module Fourteen.RestroomRedoubt where

import Control.Parallel.Strategies (parMap, rpar)
import Data.List (sortOn, transpose)

---

main :: IO ()
main = do
  inp <- input
  print $ partOne inp
  print $ partTwo inp

-- Part 1

partOne :: [Robot] -> Int
partOne robots = getSafetyScore robotsAfter100Seconds
  where
    robotsAfter100Seconds = parMap rpar (last . stepNSeconds 103 101 100) robots

-- Part 2

partTwo :: [Robot] -> Int
partTwo robots = fst $ head lowestScores
  where
    lowestScores = take 100 $ sortOn snd safetyScoresBySecond
    safetyScoresBySecond = [(second, getSafetyScore robotsThisSecond) | (second, robotsThisSecond) <- zip [0 ..] robotsStatesBySecond]
    allRobotTrajectories = parMap rpar (stepNSeconds 103 101 10000) robots
    robotsStatesBySecond = transpose allRobotTrajectories

-- Common

getSafetyScore :: [Robot] -> Int
getSafetyScore robots =
  let q1 = foldr (\((px, py), _) acc -> if px > 50 && py < 51 then acc + 1 else acc) 0 robots
      q2 = foldr (\((px, py), _) acc -> if px < 50 && py < 51 then acc + 1 else acc) 0 robots
      q3 = foldr (\((px, py), _) acc -> if px < 50 && py > 51 then acc + 1 else acc) 0 robots
      q4 = foldr (\((px, py), _) acc -> if px > 50 && py > 51 then acc + 1 else acc) 0 robots
   in q1 * q2 * q3 * q4

stepNSeconds :: Int -> Int -> Int -> Robot -> [Robot]
stepNSeconds height width seconds robot =
  take (seconds + 1) $ iterate (step height width) robot

step :: Int -> Int -> Robot -> Robot
step height width ((x, y), (vx, vy)) =
  let x' = (x + vx) `mod` width
      y' = (y + vy) `mod` height
   in ((x', y'), (vx, vy))

type Vel = (Int, Int)

type Pos = (Int, Int)

type Robot = (Pos, Vel)

input :: IO [Robot]
input = do
  rawInput <- map words . lines <$> readFile "Fourteen/input.txt"
  let parseInput inp =
        let px = read @Int $ takeWhile (/= ',') $ drop 2 (head inp)
            py = read @Int $ drop 1 $ dropWhile (/= ',') $ drop 2 (head inp)
            vx = read @Int $ takeWhile (/= ',') $ drop 2 (last inp)
            vy = read @Int $ drop 1 $ dropWhile (/= ',') $ drop 2 (last inp)
         in ((px, py), (vx, vy))
  return $ map parseInput rawInput
