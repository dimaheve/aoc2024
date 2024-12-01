module First where

inputs = lines <$> readFile "inputs.txt"

removeSpace :: String -> (String, String)
removeSpace str =
  let (firstPart, rest) = break (== ' ') str
      secondPart = filter (/= ' ') (dropWhile (== ' ') rest)
   in (firstPart, secondPart)

removeSpaces :: [String] -> ([String], [String])
removeSpaces lst = removeSpaces' lst [] []
  where
    removeSpaces' [] a b = (a, b)
    removeSpaces' (x : xs) a b =
      let (c, d) = removeSpace x
       in removeSpaces' xs (c : a) (d : b)

countRepeated :: Int -> [Int] -> Int
countRepeated x = length . filter (== x)

main :: IO ()
main = do
  inp <- inputs
  let (a, b) = removeSpaces inp
      aOfInts = map (read @Int) a
      bOfInts = map (read @Int) b
      repeats = map (`countRepeated` bOfInts) aOfInts
      mulRepeats = zipWith (*) aOfInts repeats
      sumRepeats = sum mulRepeats

  print sumRepeats
