{-# LANGUAGE OverloadedStrings #-}

module MultItOver where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.State (StateT (..))
import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text as T
  ( Text,
    drop,
    head,
    isPrefixOf,
    length,
    null,
    tail,
    takeWhile,
    unpack,
  )
import Data.Text.IO as TIO (readFile)

-- Common

input :: IO Text
input = TIO.readFile "input.txt"

type Parser = StateT Text Maybe

parseWithControl :: forall a. Parser a -> (a -> a) -> Maybe (Bool -> Parser Bool) -> Text -> [a]
parseWithControl mainParser transform controlParser = parseWithState True
  where
    parseElement :: Bool -> Text -> [a]
    parseElement state input = case runStateT mainParser input of
      Just (result, remaining) ->
        (if state then result else transform result) : parseWithState state remaining
      Nothing -> parseWithState state (T.tail input)

    parseWithState :: Bool -> Text -> [a]
    parseWithState state input
      | T.null input = []
      | otherwise =
          case controlParser of
            Just checkControl -> handleControlParser state input checkControl
            Nothing -> parseElement state input

    handleControlParser :: Bool -> Text -> (Bool -> Parser Bool) -> [a]
    handleControlParser state input control = case runStateT (control state) input of
      Just (newState, remaining) -> parseElement newState remaining
      Nothing -> parseElement state input

-- Part 1

charParse :: Char -> Parser Char
charParse ch = StateT $ \input ->
  case (T.head input, T.drop 1 input) of
    (x, xs) | x == ch -> Just (x, xs)
    _ -> Nothing

stringParse :: Text -> Parser Text
stringParse str = StateT $ \input ->
  if str `T.isPrefixOf` input
    then Just (str, T.drop (T.length str) input)
    else Nothing

numberParse :: Parser Int
numberParse = StateT $ \input ->
  let digits = T.takeWhile isDigit input
   in if T.null digits
        then Nothing
        else
          let consumedInput = T.drop (T.length digits) input
              intNums = read @Int $ T.unpack digits
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

partOne :: Text -> Int
partOne input = sum $ Prelude.map (uncurry (*)) (parseWithControl mulParser id Nothing input)

-- Part 2

doParse :: Bool -> Parser Bool
doParse status = do
  should <- stringParse "do()" <|> stringParse "don't()"
  case should of
    "do()" -> return True
    "don't()" -> return False

partTwo :: Text -> Int
partTwo input = sum $ Prelude.map (uncurry (*)) (parseWithControl mulParser (const (0, 0)) (Just doParse) input)

---

main :: IO ()
main = do
  inp <- input
  print $ partOne inp
  print $ partTwo inp
