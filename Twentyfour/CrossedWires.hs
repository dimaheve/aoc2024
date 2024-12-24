module Twentyfour.CrossedWires where

import Data.List (intercalate, nub, sort)
import Data.Map (Map)
import Data.Map qualified as Map

---

main :: IO ()
main = do
  wires <- input
  print $ partOne wires
  print $ partTwo wires

-- Part 1

partOne :: Wires -> Int
partOne wires = bitsToDecimal zBits
  where
    bitsToDecimal = foldr (\bit acc -> acc * 2 + fromEnum bit) 0
    (zBits, _) = Map.foldrWithKey collectZWires ([], wires) wires
    collectZWires name _ (bits, wiresState) =
      if head name == 'z'
        then
          let (value, newWiresState) = evalWire name wiresState
           in (value : bits, newWiresState)
        else (bits, wiresState)

-- Part 2

partTwo :: Wires -> String
partTwo wires = intercalate "," . nub . sort $ concat [incorrectZWires, invalidInternalXORs, miswiredANDGates, brokenXORChain]
  where
    internalWires = Map.filterWithKey (\k _ -> head k `notElem` "xyz") wires
    incorrectZWires =
      init
        [ name
          | (name, wire) <- Map.toList wires,
            head name == 'z',
            not $ isXORGate wire
        ]
    invalidInternalXORs =
      [ name
        | (name, wire) <- Map.toList internalWires,
          isXORGate wire,
          let inputs = gateInputs wire,
          not (any isXYWire inputs)
      ]
    miswiredANDGates =
      tail
        [ name
          | (name, wire) <- Map.toList internalWires,
            isANDGate wire,
            not $ all isORGate (parentGates name wires)
        ]
    brokenXORChain =
      [ name
        | (name, wire) <- Map.toList internalWires,
          isXORGate wire,
          hasXYInputs wire,
          not $ all (\p -> isXORGate p || isANDGate p) (parentGates name wires)
      ]

    isXORGate (WireOp (XOR _ _)) = True
    isXORGate _ = False

    isANDGate (WireOp (AND _ _)) = True
    isANDGate _ = False

    isORGate (WireOp (OR _ _)) = True
    isORGate _ = False

    isXYWire w = head w `elem` "xy"

    hasXYInputs (WireOp op) = any isXYWire (gateInputs $ WireOp op)
    hasXYInputs _ = False

    gateInputs (WireOp (XOR a b)) = [a, b]
    gateInputs (WireOp (AND a b)) = [a, b]
    gateInputs (WireOp (OR a b)) = [a, b]
    gateInputs _ = []

    parentGates name = Map.filter (hasInput name)

    hasInput _ (WireState _) = False
    hasInput name (WireOp op) = name `elem` gateInputs (WireOp op)

-- Common

evalAllWires :: Wires -> Wires
evalAllWires wires = Map.foldrWithKey (\name _ w -> snd $ evalWire name w) wires wires

evalWire :: String -> Wires -> (Bool, Wires)
evalWire name wires = case wires Map.! name of
  WireState val -> (val, wires)
  WireOp op ->
    let (val1, wires') = evalWire input1 wires
        (val2, wires'') = evalWire input2 wires'
        res = evalOp op val1 val2
     in (res, Map.insert name (WireState res) wires'')
  where
    evalOp (AND _ _) = (&&)
    evalOp (OR _ _) = (||)
    evalOp (XOR _ _) = \a b -> (a || b) && not (a && b)

    (input1, input2) = getInputs $ wires Map.! name

    getInputs (WireOp (AND i1 i2)) = (i1, i2)
    getInputs (WireOp (OR i1 i2)) = (i1, i2)
    getInputs (WireOp (XOR i1 i2)) = (i1, i2)
    getInputs (WireState _) = error "WireState has no inputs"

data Op where
  XOR :: String -> String -> Op
  AND :: String -> String -> Op
  OR :: String -> String -> Op
  deriving (Show, Eq, Ord)

data Wire where
  WireState :: Bool -> Wire
  WireOp :: Op -> Wire
  deriving (Show, Eq, Ord)

type Wires = Map String Wire

input :: IO Wires
input = do
  contents <- readFile "Twentyfour/input.txt"
  let (inputs, ops) = break null $ lines contents
  return $ parseOps (drop 1 ops) $ parseInputs inputs

parseInputs :: [String] -> Wires
parseInputs = Map.fromList . map parseInput
  where
    parseInput line = (name, WireState $ toBool value)
      where
        (name, valueStr) =
          case break (== ':') line of
            (n, ':' : ' ' : v) -> (n, v)
            bad -> error $ "invalid operation" ++ show bad
        value = read @Int valueStr
        toBool 1 = True
        toBool 0 = False
        toBool n = error $ "invalid bit: " ++ show n

parseOps :: [String] -> Wires -> Wires
parseOps ops initWires = foldr insertOp initWires ops
  where
    insertOp line = Map.insert name (WireOp op)
      where
        (op, name) = case words line of
          [in1, opStr, in2, "->", out] -> case opStr of
            "AND" -> (AND in1 in2, out)
            "OR" -> (OR in1 in2, out)
            "XOR" -> (XOR in1 in2, out)
            bad -> error $ "invalid operation: " ++ bad
          bad -> error $ "invalid operation: " ++ unwords bad
