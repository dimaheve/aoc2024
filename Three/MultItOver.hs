{-# LANGUAGE OverloadedStrings #-}

module Three.MultItOver where

import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

-- Common

input :: IO Text
input = TIO.readFile "Three/input.txt"

type Parser = Parsec Void Text

data Command = Mul (Int, Int) | Do | Dont deriving (Show)

mulParser :: Parser (Int, Int)
mulParser = do
  _ <- string "mul("
  n1 <- some digitChar
  _ <- char ','
  n2 <- some digitChar
  _ <- char ')'
  return (read n1, read n2)

commandParser :: Parser Command
commandParser =
  choice
    [ Mul <$> try mulParser,
      Do <$ string "do()",
      Dont <$ string "don't()"
    ]

parseCommands :: Parser [Command]
parseCommands = catMaybes <$> many (Just <$> try commandParser <|> Nothing <$ anySingle)

-- Part 1

partOne :: Text -> Int
partOne inp = case parse parseCommands "" inp of
  Left _ -> 0
  Right cmds -> sum [x * y | Mul (x, y) <- cmds]

-- Part 2

processCommands :: [Command] -> Int
processCommands = handle True 0
  where
    handle _ acc [] = acc
    handle counting acc (cmd : cmds) = case cmd of
      Mul (x, y) -> handle counting (if counting then acc + (x * y) else acc) cmds
      Do -> handle True acc cmds
      Dont -> handle False acc cmds

partTwo :: Text -> Int
partTwo inp = case parse parseCommands "" inp of
  Left _ -> 0
  Right cmds -> processCommands cmds

---

main :: IO ()
main = do
  inp <- input
  print $ partOne inp
  print $ partTwo inp
