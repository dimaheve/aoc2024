{-# LANGUAGE LambdaCase #-}

module Sixteen.ReindeerMaze where

import Data.Array (Array)
import Data.Array qualified as A
import Data.Array.IArray (Ix (inRange), bounds)
import Data.Graph.Inductive (Gr, spTree)
import Data.Graph.Inductive.Graph (LPath (LP), Node, mkGraph)
import Data.Graph.Inductive.Query.SP (spLength)
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map
import Data.Maybe (catMaybes, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set

---

main :: IO ()
main = do
  maze <- input
  print $ partOne maze
  print $ partTwo maze

-- Part 1

partOne :: Maze -> Int
partOne maze = minimum $ catMaybes paths
  where
    (graph, stateToNode, _) = buildGraph maze
    start = (findChar maze 'S', East)
    end = findChar maze 'E'
    endStates = [(end, dir) | dir <- [North, East, South, West]]
    startNode = stateToNode Map.! start
    endNodes = [stateToNode Map.! state | state <- endStates]
    paths = [spLength startNode endNode graph | endNode <- endNodes]

-- Part 2

partTwo :: Maze -> Int
partTwo maze = Set.size optimalPositions
  where
    (graph, stateToNode, nodeToState) = buildGraph maze
    startNode = stateToNode Map.! (findChar maze 'S', East)
    endPos = findChar maze 'E'
    endNodes = [stateToNode Map.! (endPos, dir) | dir <- [North, East, South, West]]
    targetCost = minimum $ catMaybes [spLength startNode end graph | end <- endNodes]
    optimalPositions = findOptimalPositions graph startNode endNodes targetCost nodeToState

findOptimalPositions :: MazeGraph -> Node -> [Node] -> Cost -> Map Node (Pos, Direction) -> Set Pos
findOptimalPositions graph start ends targetCost nodeToState =
    Set.fromList [pos | pos <- Map.keys posToNodes, isPositionOnOptimalPath pos]
  where
    startPaths = spTree start graph
    endsPaths = [spTree end graph | end <- ends]
    costFromStart = Map.fromList [(node, cost) | LP path <- startPaths, (node, cost) <- [head path]]
    costToEnd = Map.fromListWith min [(node, cost) | endPaths <- endsPaths, LP path <- endPaths, (node, cost) <- [head path]]
    posToNodes = Map.fromListWith (++) 
      [(pos, [node]) | (node, (pos, _)) <- Map.toList nodeToState]
    isPositionOnOptimalPath pos = any isOptimalThroughNodes
      [(fromNode, toNode) | fromNode <- posToNodes Map.! pos, toNode <- posToNodes Map.! pos]
    isOptimalThroughNodes (fromNode, toNode) =
      case (Map.lookup fromNode costFromStart, Map.lookup toNode costToEnd) of
        (Just startCost, Just endCost) -> startCost + endCost == targetCost
        _ -> False

-- Common

findChar :: Maze -> Char -> Pos
findChar maze char = head [pos | (pos, cell) <- A.assocs maze, cell == char]

isValidPos :: Maze -> Pos -> Bool
isValidPos maze pos = inRange (bounds maze) pos && maze A.! pos /= '#'

possibleMoves :: Maze -> State -> [(State, Cost)]
possibleMoves maze state =
  getTurns state
    ++ maybeToList (tryForward maze state)

getTurns :: State -> [(State, Cost)]
getTurns (pos, dir) =
  [ ((pos, turnLeft dir), 1000),
    ((pos, turnRight dir), 1000)
  ]

tryForward :: Maze -> State -> Maybe (State, Cost)
tryForward maze ((x, y), dir) =
  let (dx, dy) = directionDelta dir
      newPos = (x + dx, y + dy)
   in if isValidPos maze newPos
        then Just ((newPos, dir), 1)
        else Nothing

buildGraph :: Maze -> (MazeGraph, Map (Pos, Direction) Node, Map Node (Pos, Direction))
buildGraph maze = (graph, stateToNode, nodeToState)
  where
    validPositions = [pos | (pos, _) <- A.assocs maze, isValidPos maze pos]
    allStates = [(pos, dir) | pos <- validPositions, dir <- [North, East, South, West]]
    stateToNode = Map.fromList $ zip allStates [0 ..]
    nodeToState = Map.fromList $ zip [0 ..] allStates
    edges =
      [ (stateToNode Map.! from, stateToNode Map.! to, cost)
        | from <- allStates,
          (to, cost) <- possibleMoves maze from
      ]
    graph = mkGraph (Map.toList nodeToState) edges

input :: IO Maze
input = do
  rows <- lines <$> readFile "Sixteen/input.txt"
  let height = length rows
      width = length (head rows)
      indices = [(x, y) | y <- [0 .. height - 1], x <- [0 .. width - 1]]
      cells = concat rows
  return $ A.array ((0, 0), (width - 1, height - 1)) (zip indices cells)

data Direction = North | East | South | West
  deriving (Eq, Ord, Show)

type Pos = (Int, Int)

type State = (Pos, Direction)

type Cost = Int

type Maze = Array Pos Char

type MazeGraph = Gr State Cost

turnLeft, turnRight :: Direction -> Direction
turnLeft = \case
  North -> West
  West -> South
  South -> East
  East -> North
turnRight = \case
  North -> East
  East -> South
  South -> West
  West -> North

directionDelta :: Direction -> Pos
directionDelta = \case
  North -> (0, -1)
  South -> (0, 1)
  East -> (1, 0)
  West -> (-1, 0)
