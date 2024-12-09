module Nine.DiskFragmenter where

import Data.Foldable (toList)
import Data.List (foldl')
import Data.Maybe (isJust, isNothing)
import Data.Sequence (Seq ((:<|)))
import Data.Sequence qualified as Seq
import Data.Char (isDigit)

---

main :: IO ()
main = do
  inp <- input
  print $ partOne inp
  print $ partTwo inp

-- Part 1

collectContiguous :: Seq (Maybe Int) -> Seq (Int, Maybe Int)
collectContiguous = foldr go Seq.empty
  where
    go x ((n, y) :<| xs) | x == y = (n + 1, x) :<| xs
    go x xs = (1, x) :<| xs

compressByBlock :: Seq (Int, Maybe Int) -> Seq (Int, Maybe Int)
compressByBlock diskmap =
  let expandedDiskmap = Seq.fromList $ concatMap (uncurry replicate) diskmap
   in collectContiguous (foldr compressBlock expandedDiskmap expandedDiskmap)
  where
    compressBlock Nothing currentDiskmap = currentDiskmap
    compressBlock (Just blockId) currentDiskmap =
      case (Seq.findIndexR (== Just blockId) currentDiskmap, Seq.findIndexL (== Nothing) currentDiskmap) of
        (Just blockIndex, Just emptyIndex)
          | blockIndex > emptyIndex ->
              Seq.update blockIndex Nothing $ Seq.update emptyIndex (Just blockId) currentDiskmap
        _otherwise -> currentDiskmap

partOne :: Seq (Int, Maybe Int) -> Int
partOne = computeChecksum . compressByBlock

-- Part 2

compressBySegment :: Seq (Int, Maybe Int) -> Seq (Int, Maybe Int)
compressBySegment diskmap = foldr tryCompress diskmap candidates
  where
    candidates = filter (isJust . snd) $ toList diskmap
    tryCompress (blockSize, Just blockId) currentMap =
      case (,) <$> findBlockIndex <*> findEmptyIndex of
        Just (blockIdx, emptyIdx)
          | emptyIdx < blockIdx ->
              moveBlock blockSize blockId blockIdx emptyIdx currentMap
        _otherwise -> currentMap
      where
        findBlockIndex = Seq.findIndexR (== (blockSize, Just blockId)) currentMap
        findEmptyIndex = Seq.findIndexL (\(size, mid) -> size >= blockSize && isNothing mid) currentMap

    moveBlock size bid blockIdx emptyIdx diskmap =
      let (emptySize, _) = Seq.index diskmap emptyIdx
          step1 = Seq.update blockIdx (size, Nothing) diskmap
          step2 = Seq.update emptyIdx (size, Just bid) step1
       in if emptySize == size
            then step2
            else Seq.insertAt (emptyIdx + 1) (emptySize - size, Nothing) step2

partTwo :: Seq (Int, Maybe Int) -> Int
partTwo = computeChecksum . compressBySegment

-- Common

computeChecksum :: Seq (Int, Maybe Int) -> Int
computeChecksum = fst . foldl' checksum (0, 0)
  where
    sumIndices :: Int -> Int -> Int -> Int
    sumIndices id i n = id * (n * i + (n * (n - 1)) `div` 2)
    checksum (acc, index) (count, Nothing) = (acc, index + count)
    checksum (acc, index) (count, Just blockId) = (sumIndices blockId index count + acc, index + count)

input :: IO (Seq (Int, Maybe Int))
input = do
  contents <- readFile "Nine/input.txt"
  let diskmaps = map (read @Int . pure) $ filter isDigit contents
  return $ (Seq.fromList . parseDiskmap 0 False) diskmaps
  where
    parseDiskmap :: Int -> Bool -> [Int] -> [(Int, Maybe Int)]
    parseDiskmap _ _ [] = []
    parseDiskmap blockId isEmpty (count : rest) =
      (count, block) : parseDiskmap nextId (not isEmpty) rest
      where
        block = if isEmpty then Nothing else Just blockId
        nextId = if isEmpty then blockId else blockId + 1
