{-# LANGUAGE LambdaCase #-}

module MultItOver where

import Control.Applicative
import Control.Monad.State
import Data.Char (isDigit)
import Data.List (isInfixOf, isPrefixOf, tails)
import Data.Maybe (isJust, mapMaybe)

-- Common

input :: IO String
input = readFile "input.txt"

type Parser = StateT String Maybe

parseWithControl :: forall a. Parser a -> (a -> a) -> Maybe (Bool -> Parser Bool) -> String -> [a]
parseWithControl mainParser transform controlParser = parseWithState True
  where
    parseElement :: Bool -> String -> [a]
    parseElement state input = case runStateT mainParser input of
      Just (result, remaining) ->
        (if state then result else transform result) : parseWithState state remaining
      Nothing -> parseWithState state (tail input)

    parseWithState :: Bool -> String -> [a]
    parseWithState _ [] = []
    parseWithState state input = case controlParser of
      Just checkControl -> handleControlParser state input checkControl
      Nothing -> parseElement state input

    handleControlParser :: Bool -> String -> (Bool -> Parser Bool) -> [a]
    handleControlParser state input control = case runStateT (control state) input of
      Just (newState, remaining) -> parseElement newState remaining
      Nothing -> parseElement state input

-- Part 1

charParse :: Char -> Parser Char
charParse ch = StateT $ \case
  (x : xs) | x == ch -> Just (x, xs)
  _ -> Nothing

stringParse :: String -> Parser String
stringParse str = StateT $ \input ->
  if str `isPrefixOf` input
    then Just (str, drop (length str) input)
    else Nothing

numberParse :: Parser Int
numberParse = StateT $ \input ->
  case takeWhile isDigit input of
    [] -> Nothing
    nums ->
      let consumedInput = drop (length nums) input
          intNums = read @Int nums
       in Just (intNums, consumedInput)

mulParser :: Parser (Int, Int)
mulParser = do
  _ <- stringParse "mul"
  _ <- charParse '('
  x <- numberParse
  _ <- charParse ','
  y <- numberParse
  _ <- charParse ')'
  return (x, y)

partOne :: String -> Int
partOne input = sum $ map (uncurry (*)) (parseWithControl mulParser id Nothing input)

-- Part 2

doParse :: Bool -> Parser Bool
doParse status = do
  should <- stringParse "do()" <|> stringParse "don't()"
  case should of
    "do()" -> return True
    "don't()" -> return False

partTwo :: String -> Int
partTwo input = sum $ map (uncurry (*)) (parseWithControl mulParser (const (0, 0)) (Just doParse) input)

---

main :: IO ()
main = do
  inp <- input
  print $ partOne inp
  print $ partTwo inp
