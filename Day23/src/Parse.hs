module Parse where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text (Parser)

import Definitions

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
parseIncrement, parseDecrement, parseCopy, parseJump, parseToggle, parseInstruction :: Parser Instruction

parseIncrement = Increment <$> (string "inc " *> parseOperand)
parseDecrement = Decrement <$> (string "dec " *> parseOperand)
parseCopy = do
  _ <- string "cpy "
  op <- parseOperand
  spaces
  reg <- parseOperand 
  return $ Copy op reg
parseJump = do
  _ <- string "jnz "
  reg <- parseOperand 
  spaces
  n <- parseOperand
  return $ JumpIfNotZero reg n
parseToggle = do
  _ <- string "tgl "
  n <- parseOperand 
  return $ Toggle n

parseInstruction = choice [parseIncrement, parseDecrement, parseJump, parseCopy, parseToggle]

-- | Parses input for instructions
parseInstructions :: Text -> Either ParseError [Instruction]
parseInstructions text = sequence $ go <$> T.lines text
  where go t = parse parseInstruction (T.unpack t) t
