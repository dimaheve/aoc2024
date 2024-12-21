module Twentyone.KeypadConundrum where

import Control.Monad.State
  ( MonadState (get),
    State,
    modify,
    runState,
  )
import Control.Parallel.Strategies (parMap, rpar)
import Data.Char (isDigit)
import Data.Map (Map)
import Data.Map qualified as Map

---

main :: IO ()
main = do
  input <- readFile "Twentyone/input.txt"
  -- Part 1
  print $ getComplexityFromString input 2
  -- Part 2
  print $ getComplexityFromString input 25
  where
    getComplexityFromString :: String -> Int -> Int
    getComplexityFromString input robotDepth = sum . parMap rpar (calculateComplexity robotDepth) . lines $ input

-- Common

calculateComplexity :: Int -> String -> Int
calculateComplexity depth code = totalSequenceLength path * readNumericPart code
  where
    (path, _) = runState (findControlSequence code depth) Map.empty
    readNumericPart = read . takeWhile isDigit

totalSequenceLength :: SequenceFrequencies -> Int
totalSequenceLength = Map.foldrWithKey (\seq freq acc -> length seq * freq + acc) 0

findPosition :: KeypadGrid -> Char -> GridPosition
findPosition grid c = head [(x, y) | (row, y) <- zip grid [0 ..], (ch, x) <- zip row [0 ..], ch == c]

findControlSequence :: String -> Int -> KeypadM SequenceFrequencies
findControlSequence code 0 = optimizeMovements doorKeypad (Map.singleton code 1)
findControlSequence code depth = findControlSequence code (depth - 1) >>= optimizeMovements controlKeypad

optimizeMovements :: KeypadGrid -> SequenceFrequencies -> KeypadM SequenceFrequencies
optimizeMovements grid current = do
  let blankY = snd $ findPosition grid ' '
  sequences <- concat <$> mapM (generateMovementPairs grid blankY) (Map.toList current)
  return $ foldr (uncurry $ Map.insertWith (+)) Map.empty sequences

generateMovementPairs :: KeypadGrid -> Int -> (String, Int) -> KeypadM [(String, Int)]
generateMovementPairs grid blankY (step, count) =
  mapM (\(a, b) -> (,count) <$> determineMovement grid a b blankY) $ zip step (tail step ++ [head step])

determineMovement :: KeypadGrid -> Char -> Char -> Int -> KeypadM String
determineMovement grid start end blankY
  | start == end = return "A"
  | null stepX = return $ stepY ++ "A"
  | null stepY = return $ stepX ++ "A"
  | startX == 0 && endY == blankY = return $ stepX ++ stepY ++ "A"
  | startY == blankY && endX == 0 = return $ stepY ++ stepX ++ "A"
  | otherwise = chooseBetterMovement (stepX ++ stepY ++ "A") (stepY ++ stepX ++ "A")
  where
    (startX, startY) = findPosition grid start
    (endX, endY) = findPosition grid end
    stepX = replicate (abs $ endX - startX) (if startX < endX then '>' else '<')
    stepY = replicate (abs $ endY - startY) (if startY < endY then 'v' else '^')

chooseBetterMovement :: String -> String -> KeypadM String
chooseBetterMovement a b = do
  memo <- get
  case Map.lookup key memo of
    Just result -> return result
    Nothing -> do
      modify (Map.insert key a)
      isABetter <- findOptimalChoice (Map.singleton a 1) (Map.singleton b 1)
      let better = if isABetter then a else b
      modify (Map.insert key better)
      return better
  where
    key = a ++ "," ++ b
    findOptimalChoice pathA pathB = do
      nextA <- optimizeMovements controlKeypad pathA
      nextB <- optimizeMovements controlKeypad pathB
      if totalSequenceLength nextA == totalSequenceLength nextB
        then findOptimalChoice nextA nextB
        else return $ totalSequenceLength nextA < totalSequenceLength nextB

doorKeypad, controlKeypad :: KeypadGrid
doorKeypad = [['7', '8', '9'], ['4', '5', '6'], ['1', '2', '3'], [' ', '0', 'A']]
controlKeypad = [[' ', '^', 'A'], ['<', 'v', '>']]

type KeypadGrid = [[Char]]

type SequenceFrequencies = Map String Int

type GridPosition = (Int, Int)

type MovementChoices = Map String String

type KeypadM = State MovementChoices
