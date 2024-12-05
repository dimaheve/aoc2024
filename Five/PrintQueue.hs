module Five.PrintQueue where

import Control.Monad (void)
import Data.List (intersect, (\\))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Void (Void)
import Text.Megaparsec (Parsec, optional, parse, some)
import Text.Megaparsec.Char (char, digitChar, newline)

-- Common

type Parser = Parsec Void Text

rawInput :: IO Text
rawInput = TIO.readFile "Five/input.txt"

parseRule :: Parser (Int, Int)
parseRule = do
  a <- some digitChar
  void $ char '|'
  b <- some digitChar <* optional newline
  return (read a, read b)

parseUpdate :: Parser [Int]
parseUpdate = do
  firstNum <- read @Int <$> some digitChar
  nums <- map (read @Int) <$> some (char ',' *> some digitChar) <* optional newline
  return $ firstNum : nums

parseInput :: Parser (Map Int [Int], [[Int]])
parseInput = do
  rawRules <- some parseRule
  let rules = foldr (\(k, v) m -> Map.insertWith (++) k [v] m) Map.empty rawRules
  void newline
  updates <- some parseUpdate
  return (rules, updates)

input :: IO (Map Int [Int], [[Int]])
input = do
  inp <- rawInput
  case parse parseInput "" inp of
    Left _ -> return (Map.empty, []) -- No error handling
    Right (rules, updates) -> return (rules, updates)

-- Part 1

isUpdateValid :: Map Int [Int] -> [Int] -> Bool
isUpdateValid rules update = go update []
  where
    go [] _ = True
    go (x : xs) seen = case Map.lookup x rules of
      Nothing -> go xs (x : seen)
      Just rs -> null (seen `intersect` rs) && go xs (x : seen)

removeInvalids :: (Map Int [Int], [[Int]]) -> [[Int]]
removeInvalids (rules, updates) = filter (isUpdateValid rules) updates

getMiddles :: [[Int]] -> [Int]
getMiddles = map (\upd -> upd !! (length upd `div` 2))

partOne :: (Map Int [Int], [[Int]]) -> Int
partOne (rules, updates) = sum . getMiddles . removeInvalids $ (rules, updates)

-- Part 2

fixUpdate :: Map Int [Int] -> [Int] -> [Int]
fixUpdate rules = reverse . go
  where
    go [] = []
    go (x : xs) = case Map.lookup x rules of
      Nothing -> x : go xs
      Just rs ->
        let violatingPrecedences = rs `intersect` (x : xs)
         in if null violatingPrecedences
              then x : go xs
              else go (xs ++ [x])

partTwo :: (Map Int [Int], [[Int]]) -> Int
partTwo (rules, updates) =
  let incorrectlyOrdered = updates \\ removeInvalids (rules, updates)
      fixedUpdates = map (fixUpdate rules) incorrectlyOrdered
   in sum $ getMiddles fixedUpdates

---

main :: IO ()
main = do
  inp <- input
  print $ partOne inp
  print $ partTwo inp
