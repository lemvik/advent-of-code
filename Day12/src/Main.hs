module Main where

import Data.Array.IArray
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Parsec
import Text.Parsec.Text (Parser)
import System.Environment (getArgs)

main :: IO ()
main = do
  [path] <- getArgs
  contents <- TIO.readFile path
  print $ do
    prog <- readProgram contents
    return (runProgram prog freshMachine, runProgram prog warmedUpMachine)

-- | Types of registers
data Reg = RegA | RegB | RegC | RegD
         deriving (Show, Eq)

-- | Virtual machine supports 4 registers - A, B, C, D and immediate int values
data Operand = Constant Int
             | Register Reg
             deriving (Show, Eq)

-- | Machine state consists of registers state + program counter
data MachineState = MachineState { pcounter :: Int
                                 , registerA :: Int
                                 , registerB :: Int
                                 , registerC :: Int
                                 , registerD :: Int
                                 }
                  deriving (Show, Eq)

-- | Possible instructions of the machine
data Instruction = Copy Operand Reg
                 | Increment Reg
                 | Decrement Reg
                 | JumpIfNotZero Operand Int
                 deriving (Show, Eq)

-- | Program is actually an array of instructions
newtype Program = Program (Array Int Instruction)
                deriving (Show, Eq)

-- | Parser for number
parseNumber :: Parser Int
parseNumber = do
  sign <- option 1 (do { _ <- char '-' ; return (-1) })
  num <- read <$> many1 digit
  return $ sign * num

-- | Parses a register
parseRegister :: Parser Reg
parseRegister = do
  r <- oneOf "abcd"
  case r of
    'a' -> return RegA
    'b' -> return RegB
    'c' -> return RegC
    'd' -> return RegD
    _   -> fail "Unable to parse register."

-- | Parses an operand - either register or constant
parseOperand :: Parser Operand
parseOperand = (Constant <$> try parseNumber) <|> (Register <$> parseRegister)

-- | Parsers for instructions
parseIncrement, parseDecrement, parseCopy, parseJump, parseInstruction :: Parser Instruction

parseIncrement = Increment <$> (string "inc " *> parseRegister)
parseDecrement = Decrement <$> (string "dec " *> parseRegister)
parseCopy = do
  _ <- string "cpy "
  op <- parseOperand
  spaces
  reg <- parseRegister
  return $ Copy op reg
parseJump = do
  _ <- string "jnz "
  reg <- parseOperand 
  spaces
  n <- parseNumber
  return $ JumpIfNotZero reg n

parseInstruction = choice [parseIncrement, parseDecrement, parseJump, parseCopy]

-- | Parses input for instructions
parseInstructions :: Text -> Either ParseError [Instruction]
parseInstructions text = sequence $ go <$> T.lines text
  where go t = parse parseInstruction (T.unpack t) t

-- | Creates program from instructions
readProgram :: Text -> Either ParseError Program
readProgram txt = do
  instructions <- parseInstructions txt
  return $ Program $ listArray (0, length instructions - 1) instructions


-- | Sets value of the register
setRegister :: MachineState -> Reg -> Int -> MachineState
setRegister state RegA v = state { registerA = v }
setRegister state RegB v = state { registerB = v }
setRegister state RegC v = state { registerC = v }
setRegister state RegD v = state { registerD = v }

-- | Gets the value of the register
getRegister :: MachineState -> Reg -> Int
getRegister state RegA = registerA state
getRegister state RegB = registerB state
getRegister state RegC = registerC state
getRegister state RegD = registerD state

-- | Applies delta to given register
deltaToRegister :: MachineState -> Reg -> Int -> MachineState
deltaToRegister state reg delta = setRegister state reg (getRegister state reg + delta)

-- | Applies delta to program counter
deltaToCounter :: MachineState -> Int -> MachineState
deltaToCounter state delta = state { pcounter = pcounter state + delta }

-- | Increments program counter of the machine
incrementCounter :: MachineState -> MachineState
incrementCounter state = deltaToCounter state 1

-- | Execute single instruction.
executeInstruction :: MachineState -> Instruction -> MachineState
executeInstruction state (Copy (Register source) target) = incrementCounter $ setRegister state target (getRegister state source)
executeInstruction state (Copy (Constant con) target) = incrementCounter $ setRegister state target con
executeInstruction state (Increment target) = incrementCounter $ deltaToRegister state target 1
executeInstruction state (Decrement target) = incrementCounter $ deltaToRegister state target (-1)
executeInstruction state (JumpIfNotZero source delta) = if value source == 0
                                                        then incrementCounter state
                                                        else deltaToCounter state delta
  where value (Constant v) = v
        value (Register src) = getRegister state src

-- | Fresh machine (with PC and registers at nulls)
freshMachine :: MachineState
freshMachine = MachineState 0 0 0 0 0

-- | Warmed up machine has 1 in its C register
warmedUpMachine :: MachineState
warmedUpMachine = MachineState 0 0 0 1 0

-- | Returns instruction at given offset into program's code if offset is valid
instructionAt :: Program -> Int -> Maybe Instruction
instructionAt (Program instr) pos = let (low, high) = bounds instr
                                    in if pos >= low && pos <= high
                                       then Just $ instr ! pos
                                       else Nothing

-- | Runs program using initial machine state
runProgram :: Program -> MachineState -> MachineState
runProgram prog state = case instructionAt prog (pcounter state) of
  Just instr -> runProgram prog (executeInstruction state instr)
  Nothing    -> state
