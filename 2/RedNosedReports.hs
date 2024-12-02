module RedNosedReports where

-- Common

report :: IO [[Int]]
report = do
  rawReport <- lines <$> readFile "report.txt"
  let spacedReport = map words rawReport
      parsedReport = map (map $ read @Int) spacedReport
  return parsedReport

-- Part 1

data Level where
  Init :: Int -> Level
  Asc :: Int -> Level
  Desc :: Int -> Level

isSafe :: [Int] -> Bool
isSafe [] = False
isSafe (x : xs) = isSafe' xs (Init x)
  where
    isSafe' :: [Int] -> Level -> Bool
    isSafe' [] _ = True
    isSafe' (x : xs) (Init p)
      | x == p || abs (x - p) > 3 = False
      | x > p = isSafe' xs (Asc x)
      | x < p = isSafe' xs (Desc x)
    isSafe' (x : xs) (Asc p)
      | x <= p || abs (x - p) > 3 = False
      | otherwise = isSafe' xs (Asc x)
    isSafe' (x : xs) (Desc p)
      | x >= p || abs (x - p) > 3 = False
      | otherwise = isSafe' xs (Desc x)

partOne :: [[Int]] -> Int
partOne = length . filter id . map isSafe

-- Part 2

removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (i + 1) xs

removedOne :: [a] -> [[a]]
removedOne xs = [removeAt i xs | i <- [0 .. length xs - 1]]

isSafeWithDamp :: [Int] -> Bool
isSafeWithDamp = foldr (\rl prl -> isSafe rl || prl) False . removedOne

partTwo :: [[Int]] -> Int
partTwo = length . filter id . map isSafeWithDamp

---

main :: IO ()
main = do
  rep <- report
  print $ partOne rep
  print $ partTwo rep
