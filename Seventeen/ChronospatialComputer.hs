module Seventeen.ChronospatialComputer where

import Control.Applicative ((<|>))
import Data.Bits (Bits (xor), shiftL)
import Data.Char (isDigit)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

---

main :: IO ()
main = do
  inp <- input
  print $ partOne inp
  print $ partTwo inp

-- Part 1

partOne :: (Registers, [Int]) -> String
partOne (initialRegs, program) =
  intercalate "," $ map show $ runProgram initialRegs program

-- Part 2

partTwo :: (Registers, [Int]) -> Int
partTwo (_, prog) = fromMaybe (error "no solution found") $ getQuineAReg (length prog - 1) 0
  where
    getQuineAReg index currentAReg =
      let tryCandidate candidate =
            let potentialA = currentAReg * 8 + candidate
                output = runProgram (Registers potentialA 0 0) prog
             in case (output == drop index prog, index == 0) of
                  (False, _) -> Nothing
                  (True, True) -> Just potentialA
                  (True, False) -> getQuineAReg (index - 1) potentialA
       in foldr (\c acc -> acc <|> tryCandidate c) Nothing [0 .. 7]

-- Common

data Machine where Machine :: {registers :: Registers, ip :: Int} -> Machine

data Registers where Registers :: {regA :: Int, regB :: Int, regC :: Int} -> Registers deriving (Show)

data Instruction = ADV | BXL | BST | JNZ | BXC | OUT | BDV | CDV deriving (Show, Eq, Enum)

runProgram :: Registers -> [Int] -> [Int]
runProgram regs prog = reverse $ go (Machine regs 0) []
  where
    go machine acc
      | ip machine >= length prog = acc
      | otherwise =
          let opcode = prog !! ip machine
              operand = prog !! (ip machine + 1)
              (newMachine, maybeOutput) = execInstruction machine (opcode, operand)
           in case maybeOutput of
                Just out -> go newMachine (out : acc)
                Nothing -> go newMachine acc

evalCombo :: Registers -> Int -> Int
evalCombo regs n
  | n <= 3 = n
  | n == 4 = regA regs
  | n == 5 = regB regs
  | n == 6 = regC regs
  | otherwise = error "invalid combo operand"

execInstruction :: Machine -> (Int, Int) -> (Machine, Maybe Int)
execInstruction machine (opcode, operand) =
  let regs = registers machine
      nextIp = ip machine + 2
      mNextIp = machine {ip = nextIp}
   in case toEnum opcode of
        ADV -> (mNextIp {registers = updateA (registers mNextIp) result}, Nothing)
          where
            result = divByPower2 (regA regs) (evalCombo regs operand)
        BXL -> (mNextIp {registers = updateB (registers mNextIp) result}, Nothing)
          where
            result = regB regs `xor` operand
        BST -> (mNextIp {registers = updateB (registers mNextIp) result}, Nothing)
          where
            result = evalCombo regs operand `mod` 8
        JNZ ->
          if regA regs /= 0
            then (machine {ip = operand}, Nothing)
            else (mNextIp, Nothing)
        BXC -> (mNextIp {registers = updateB (registers mNextIp) result}, Nothing)
          where
            result = regB regs `xor` regC regs
        OUT -> (mNextIp, Just output)
          where
            output = evalCombo regs operand `mod` 8
        BDV -> (mNextIp {registers = updateB (registers mNextIp) result}, Nothing)
          where
            result = divByPower2 (regA regs) (evalCombo regs operand)
        CDV -> (mNextIp {registers = updateC (registers mNextIp) result}, Nothing)
          where
            result = divByPower2 (regA regs) (evalCombo regs operand)
  where
    updateA r n = r {regA = n}
    updateB r n = r {regB = n}
    updateC r n = r {regC = n}
    power2 n = 1 `shiftL` n
    divByPower2 num pow = num `div` power2 pow

input :: IO (Registers, [Int])
input = do
  rawInput <- readFile "Seventeen/input.txt"
  let ls = lines rawInput
      regA = parseRegister $ head ls
      regB = parseRegister $ ls !! 1
      regC = parseRegister $ ls !! 2
      prog = parseProg $ last ls
  return (Registers regA regB regC, prog)
  where
    parseRegister = read . takeWhile isDigit . dropWhile (not . isDigit)
    parseProg = map read . splitOn "," . drop 9
