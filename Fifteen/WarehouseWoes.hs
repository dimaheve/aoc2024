{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Fifteen.WarehouseWoes where

import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

---

main :: IO ()
main = do
  (initialGameState, moves) <- input
  print $ partOne initialGameState moves
  print $ partTwo initialGameState moves

-- Part 1

partOne :: WarehouseState -> [Direction] -> Int
partOne initialGameState moves = calculateGPSCoordinates $ foldl' tryMove initialGameState moves

-- Part 2

partTwo :: WarehouseState -> [Direction] -> Int
partTwo initialGameState moves = calculateGPSCoordinates $ foldl' tryMove (doubleWarehouse initialGameState) moves

doubleWarehouse :: WarehouseState -> WarehouseState
doubleWarehouse WarehouseState {..} =
  WarehouseState
    { walls = Set.map (\(r, c) -> (r, c * 2)) walls `Set.union` Set.map (\(r, c) -> (r, c * 2 + 1)) walls,
      boxes = Map.fromList $ concatMap makeWideBox $ Map.toList boxes,
      robot = (fst robot, snd robot * 2),
      dims = (fst dims, snd dims * 2)
    }
  where
    makeWideBox ((r, c), SmallBox) =
      [ ((r, c * 2), WideBox LeftBox),
        ((r, c * 2 + 1), WideBox RightBox)
      ]
    makeWideBox _ = error "shouldn't have wide boxes in input"

-- Common

calculateGPSCoordinates :: WarehouseState -> Int
calculateGPSCoordinates WarehouseState {boxes} =
  sum
    [ r * 100 + c
      | ((r, c), box) <- Map.toList boxes,
        case box of
          SmallBox -> True
          WideBox LeftBox -> True
          WideBox RightBox -> False
    ]

tryMove :: WarehouseState -> Direction -> WarehouseState
tryMove (WarehouseState {..}) direction =
  case (nextRobotPosition `elem` walls, nextRobotPosition `Map.member` boxes) of
    (True, _) -> WarehouseState {..}
    (False, False) -> WarehouseState {robot = nextRobotPosition, ..}
    (False, True) -> fromMaybe WarehouseState {..} pushedGameState
  where
    pushedGameState = do
      connectedBoxes <- findConnectedBoxes robot Set.empty
      let shiftedBoxes = Map.mapKeys shiftBox boxes
          shiftBox pos = if pos `Set.member` connectedBoxes then move pos direction else pos
      return $ WarehouseState {boxes = shiftedBoxes, robot = nextRobotPosition, ..}
    findConnectedBoxes startPos seen =
      let nextPos = move startPos direction
          handleWideBox side = do
            let otherPos = case side of
                  LeftBox -> (fst nextPos, snd nextPos + 1)
                  RightBox -> (fst nextPos, snd nextPos - 1)
            pathA <- findConnectedBoxes nextPos (Set.insert nextPos seen)
            pathB <- findConnectedBoxes otherPos (Set.insert otherPos seen)
            Just (pathA `Set.union` pathB)
       in case (Map.lookup nextPos boxes, nextPos `elem` walls) of
            (_, True) -> Nothing
            (Nothing, False) -> Just seen
            (Just box, False) ->
              case (direction, box) of
                (W, _) -> findConnectedBoxes nextPos (Set.insert nextPos seen)
                (E, _) -> findConnectedBoxes nextPos (Set.insert nextPos seen)
                (_, WideBox side) -> handleWideBox side
                (_, SmallBox) -> findConnectedBoxes nextPos (Set.insert nextPos seen)
    nextRobotPosition = move robot direction
    move (r, c) = \case
      N -> (r - 1, c)
      S -> (r + 1, c)
      W -> (r, c - 1)
      E -> (r, c + 1)

input :: IO (WarehouseState, [Direction])
input = do
  rawInput <- readFile "Fifteen/input.txt"
  let (mapStr, movesStr) = break null $ lines rawInput
      dims = (length mapStr, length $ head mapStr)
      walls = Set.fromList [(r, c) | (r, row) <- zip [0 ..] mapStr, (c, ch) <- zip [0 ..] row, ch == '#']
      boxes = Map.fromList [((r, c), SmallBox) | (r, row) <- zip [0 ..] mapStr, (c, ch) <- zip [0 ..] row, ch == 'O']
      robot = head [(r, c) | (r, row) <- zip [0 ..] mapStr, (c, ch) <- zip [0 ..] row, ch == '@']
      moves = map parseDir $ filter (`elem` "<>^v") $ concat $ drop 1 movesStr
  return (WarehouseState {walls, boxes, robot, dims}, moves)
  where
    parseDir = \case '^' -> N; 'v' -> S; '<' -> W; '>' -> E; x -> error $ "invalid direction: " ++ [x]

data Direction = N | S | W | E deriving (Show, Eq, Ord)

data WarehouseState where
  WarehouseState ::
    { walls :: Set Pos,
      boxes :: Map Pos Box,
      robot :: Pos,
      dims :: (Int, Int)
    } ->
    WarehouseState
  deriving (Show, Eq, Ord)

type Pos = (Int, Int)

data Box where
  SmallBox :: Box
  WideBox :: WideBox -> Box
  deriving (Show, Eq, Ord)

data WideBox = LeftBox | RightBox deriving (Show, Eq, Ord)
