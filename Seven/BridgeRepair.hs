module Seven.BridgeRepair where

import Control.Monad (replicateM)

-- Common

data Ops = Add | Mul | Con deriving (Show)

type Equation = (Int, [Int])

input :: IO [Equation]
input = do
  ls <- lines <$> readFile "Seven/input.txt"
  pure $ map parseLine ls
  where
    parseLine l =
      let (target, rest) = break (== ':') l
          nums = words $ drop 2 rest
       in (read target, map read nums)

compute :: [Ops] -> [Int] -> Maybe Int
compute [] [n] = Just n
compute (op : ops) (a : b : ns) = do
  result <- case op of
    Add -> Just (a + b)
    Mul -> Just (a * b)
    Con -> Just $ read (show a ++ show b)
  compute ops (result : ns)
compute _ _ = Nothing

correctOps :: Equation -> Bool -> Bool
correctOps (test, nums) withConcat =
  any (\op -> compute op nums == Just test) $
    replicateM (length nums - 1) ops
  where
    ops = if withConcat then [Add, Mul, Con] else [Add, Mul]

solve :: Bool -> [Equation] -> Int
solve withConcat = sum . map fst . filter (`correctOps` withConcat)

-- Part 1

partOne :: [Equation] -> Int
partOne = solve False

-- Part 2

partTwo :: [Equation] -> Int
partTwo = solve True

---

main :: IO ()
main = do
  inp <- input
  print $ partOne inp
  print $ partTwo inp
