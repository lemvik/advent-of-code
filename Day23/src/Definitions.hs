module Definitions where

-- | Types of registers
data Reg = RegA | RegB | RegC | RegD
         deriving (Show, Eq)

-- | Virtual machine supports 4 registers - A, B, C, D and immediate int values
data Operand = Constant Int
             | Register Reg
             deriving (Show, Eq)

-- | Possible instructions of the machine
data Instruction = Copy Operand Operand 
                 | Increment Operand 
                 | Decrement Operand 
                 | JumpIfNotZero Operand Operand 
                 | Toggle Operand
                 deriving (Show, Eq)
