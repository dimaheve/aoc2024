module RedNosedReports where

-- Common

report :: IO [[Int]]
report = do
  rawReport <- lines <$> readFile "report.txt"
  let spacedReport = map words rawReport
      parsedReport = map (map $ read @Int) spacedReport
  return parsedReport

-- Part 1

data Safe where
  InitSafe :: Int -> Safe
  AscSafe :: Int -> Safe
  DescSafe :: Int -> Safe

isSafe :: [Int] -> Bool
isSafe [] = False
isSafe (x : xs) = isSafe' xs (InitSafe x)
  where
    isSafe' :: [Int] -> Safe -> Bool
    isSafe' [] _ = True
    isSafe' (x : xs) (InitSafe p)
      | x == p || abs (x - p) > 3 = False
      | x > p = isSafe' xs (AscSafe x)
      | x < p = isSafe' xs (DescSafe x)
    isSafe' (x : xs) (AscSafe p)
      | x <= p || abs (x - p) > 3 = False
      | otherwise = isSafe' xs (AscSafe x)
    isSafe' (x : xs) (DescSafe p)
      | x >= p || abs (x - p) > 3 = False
      | otherwise = isSafe' xs (DescSafe x)

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
