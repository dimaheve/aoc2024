module Four.CeresSearch where

import Data.Bifunctor (Bifunctor (bimap))

-- Common

input :: IO [String]
input = lines <$> readFile "Four/input.txt"

type Coords = (Int, Int)

-- Part 1

getRows :: [a] -> Int
getRows = length

getCols :: (Foldable t) => [t a] -> Int
getCols grid = length $ head grid

getAllPositions :: (Int, Int) -> [Coords]
getAllPositions (rows, cols) = [(r, c) | r <- [0 .. rows - 1], c <- [0 .. cols - 1]]

checkBoundsWithDelta :: (Int, Int) -> Coords -> Int -> Int -> Int -> Bool
checkBoundsWithDelta (rows, cols) (r, c) dr dc i =
  r + i * dr >= 0
    && r + i * dr < rows
    && c + i * dc >= 0
    && c + i * dc < cols

getWords :: [String] -> (Int, Int) -> [(Int, Int)] -> Int -> Coords -> [String]
getWords grid (rows, cols) directions len (r, c) =
  [ [grid !! (r + i * dr) !! (c + i * dc) | i <- [0 .. len - 1]]
    | (dr, dc) <- directions,
      all (checkBoundsWithDelta (rows, cols) (r, c) dr dc) [0 .. len - 1]
  ]

partOne :: [String] -> Int
partOne inp = length $ concatMap (filter (== "XMAS") . getWordsByPosition) allPositions
  where
    rows = getRows inp
    cols = getCols inp
    allPositions = getAllPositions (rows, cols)

    getWordsByPosition =
      getWords inp (rows, cols) [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1]] 4

-- Part 2

partTwo :: [String] -> Int
partTwo inp = length $ filter id (map isXMas $ getAllPositions (getRows inp, getCols inp))
  where
    isXMas (r, c) =
      let diagonals = [(x, y) | x <- [-1, 1], y <- [-1, 1]]
          corners = map (bimap (+ r) (+ c)) (reverse diagonals)

          wordsFromCorners =
            concat $ zipWith getWordInDirection diagonals corners

          getWordInDirection direction =
            getWords inp (getRows inp, getCols inp) [direction] 3
       in length (filter (== "MAS") wordsFromCorners) >= 2

---

main :: IO ()
main = do
  inp <- input
  print $ partOne inp
  print $ partTwo inp
