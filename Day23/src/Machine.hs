{-# LANGUAGE BangPatterns #-}
module Machine where

import Definitions
import Control.Monad.ST.Strict
import Control.Monad
import Data.STRef
import Data.Array.ST.Safe
import Data.Array.IArray

-- | Type for program
type Program = Array Int Instruction

-- | Machine state consists of registers state + program counter
data MachineState s = MachineState { pcounter :: STRef s Int
                                   , registerA :: STRef s Int
                                   , registerB :: STRef s Int
                                   , registerC :: STRef s Int
                                   , registerD :: STRef s Int
                                   , program :: STArray s Int Instruction }
                  deriving (Eq)

-- | Immutable machine image
data MachineImage = MachineImage { ipcounter :: Int
                                 , iregisterA :: Int
                                 , iregisterB :: Int
                                 , iregisterC :: Int
                                 , iregisterD :: Int
                                 , iprogram :: Program }
                  deriving (Show, Eq)

-- | Get reference to the register
registerRef :: MachineState s -> Reg -> STRef s Int
registerRef (MachineState _ !ra _ _ _ _) RegA = ra
registerRef (MachineState _ _ !rb _ _ _) RegB = rb
registerRef (MachineState _ _ _ !rc _ _) RegC = rc
registerRef (MachineState _ _ _ _ !rd _) RegD = rd

-- | Sets value of the register
setRegister :: MachineState s -> Reg -> Int -> ST s ()
setRegister !state reg v = do
  () <- modifySTRef' (registerRef state reg) (const v)
  return ()

-- | Gets the value of the register
getRegister :: MachineState s -> Reg -> ST s Int
getRegister !state reg = do
  !val <- readSTRef (registerRef state reg)
  return val

-- | Returns the current program counter
getPC :: MachineState s -> ST s Int
getPC (MachineState !pc _ _ _ _ _) = do
  !pc' <- readSTRef pc 
  return pc'

-- | Applies delta to given register
deltaToRegister :: MachineState s -> Reg -> Int -> ST s ()
deltaToRegister !state !reg !delta = do
  () <- modifySTRef' (registerRef state reg) (+ delta)
  return ()

-- | Applies delta to program counter
deltaToCounter :: MachineState s -> Int -> ST s ()
deltaToCounter (MachineState !pc _ _ _ _ _) !delta = do
  () <- modifySTRef' pc (+ delta)
  return ()

-- | Increments program counter of the machine
incrementCounter :: MachineState s -> ST s () 
incrementCounter !state = deltaToCounter state 1

-- | Boots machine with given program as initial state
bootMachine :: MachineImage -> ST s (MachineState s)
bootMachine (MachineImage ipc irA irB irC irD iprg) = do
  pc <- newSTRef ipc 
  regA <- newSTRef irA 
  regB <- newSTRef irB
  regC <- newSTRef irC 
  regD <- newSTRef irD 
  prog <- thaw iprg
  return $ MachineState pc regA regB regC regD prog

-- | Stops the machine and makes snapshot of its state
stopMachine :: MachineState s -> ST s MachineImage
stopMachine !state = do
  pc <- getPC state
  regA <- getRegister state RegA
  regB <- getRegister state RegB
  regC <- getRegister state RegC
  regD <- getRegister state RegD
  prog <- freeze (program state)
  return $ MachineImage pc regA regB regC regD prog

-- | Returns true if machine has halted (i.e. program counter ran off the program)
halted :: MachineState s -> ST s Bool
halted (MachineState !pc _ _ _ _ !pr) = do
  !pc' <- readSTRef pc
  !(l, u) <- getBounds pr
  return $ l > pc' || u < pc' 

-- | Utility to obtain operand value
operandValue :: MachineState s -> Operand -> ST s Int
operandValue _     (Constant c) = return c
operandValue !state (Register r) = getRegister state r

runInstruction :: MachineState s -> Instruction -> ST s ()
runInstruction !state (Copy op (Register r)) = do
  !val <- operandValue state op
  setRegister state r val
  incrementCounter state
runInstruction !state (Copy _ (Constant _)) = incrementCounter state
runInstruction !state (Increment (Register reg)) = do
  deltaToRegister state reg 1
  incrementCounter state
runInstruction !state (Increment (Constant _)) = incrementCounter state
runInstruction !state (Decrement (Register reg)) = do
  deltaToRegister state reg (-1)
  incrementCounter state
runInstruction !state (Decrement (Constant _)) = incrementCounter state
runInstruction !state (JumpIfNotZero op n) = do
  !val <- operandValue state op
  !nval <- operandValue state n
  if val /= 0
    then deltaToCounter state nval
    else incrementCounter state
runInstruction !state (Toggle op) = do
  !val <- operandValue state op
  !pc <- getPC state
  !bnds <- getBounds (program state)
  when (inRange bnds (pc + val)) $ toggleInstruction state (pc + val)
  incrementCounter state

-- | Gets instruction at given address
getInstruction :: MachineState s -> Int -> ST s Instruction
getInstruction !state = do
  !instr <- readArray (program state)
  return instr

-- | Gets the instruction program counter points to
getCurrentInstruction :: MachineState s -> ST s Instruction
getCurrentInstruction !state = getPC state >>= getInstruction state

-- | Modifies program by toggling instruction at given point
toggleInstruction :: MachineState s -> Int -> ST s ()
toggleInstruction !state p = do
  instruction <- getInstruction state p
  () <- writeArray (program state) p (transformInstruction instruction) 
  return ()

-- | Transforms the instruction via rules for toggling.
transformInstruction :: Instruction -> Instruction
transformInstruction (Increment r) = Decrement r
transformInstruction (Decrement r) = Increment r
transformInstruction (Toggle op) = Increment op
transformInstruction (Copy op r) = JumpIfNotZero op r
transformInstruction (JumpIfNotZero op r) = Copy op r

-- | Runs program until it halts
run :: MachineState s -> ST s ()
run !state = do
  !stop <- halted state
  unless stop $ do
    !instruction <- getCurrentInstruction state
    runInstruction state instruction
    run state

-- | Runs an image and returns 
runImage :: MachineImage -> MachineImage
runImage img = runST $ do
  !machine <- bootMachine img
  run machine
  stopMachine machine
