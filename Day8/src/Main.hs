module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as IO
import Text.Parsec
import Text.Parsec.Text
import System.Environment (getArgs)
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.Foldable (sum)

main :: IO ()
main = do
  [path, width', height'] <- getArgs
  instructions <- readInstructions path
  let width = read width'
      height = read height'
  case instructions of
    Left err -> print err
    --Right instructions' -> print $ runST (runProgram countBits (width, height) instructions')
    Right instructions' -> sequence_ $ putStrLn <$> runST (runProgram showState (width, height) instructions')

-- | Alias for row
type Row = Int

-- | Alias for column
type Col = Int

-- | Type representing instruction
data Instruction = RotateRow Row Int
                 | RotateColumn Col Int
                 | CreateRect Int Int
                 deriving (Show, Eq)

-- | Type representing dimensions of the display.
type Dimensions = (Int, Int)

-- | Type representing display state
data DisplayState s = DS { contents :: STArray s Int Bool
                         , dimensions :: Dimensions
                         }
                      deriving (Eq)

-- | Parses rotate row instruction
parseRotateRow :: Parser Instruction
parseRotateRow = do
  _ <- string "rotate row y="
  row <- read <$> many1 digit
  _ <- string " by "
  delta <- read <$> many1 digit
  return $ RotateRow row delta

-- | Parses rotate column instruction
parseRotateColumn :: Parser Instruction
parseRotateColumn = do
  _ <- string "rotate column x="
  col <- read <$> many1 digit
  _ <- string " by "
  delta <- read <$> many1 digit
  return $ RotateColumn col delta
  
-- | Parses create rectange instruction
parseCreateRect :: Parser Instruction
parseCreateRect = do
  _ <- string "rect "
  rows <- read <$> many1 digit
  _ <- string "x"
  cols <- read <$> many1 digit
  return $ CreateRect rows cols

-- | Parses a list of instructions
parseInstruction :: Parser Instruction
parseInstruction = try parseRotateRow <|> try parseRotateColumn <|> parseCreateRect

-- | Reads instructions from given file.
readInstructions :: FilePath -> IO (Either ParseError [Instruction])
readInstructions path = do
  cont <- T.lines <$> IO.readFile path
  return $ sequence $ parse parseInstruction path <$> cont

-- | Creates initially empty display
createDisplay :: Dimensions -> ST s (DisplayState s)
createDisplay dims = do
  a <- newArray (0, uncurry (*) dims) False
  return $ DS a dims

-- | Processes instruction and changes the display accordingly
processInstruction :: DisplayState s -> Instruction -> ST s (DisplayState s)
processInstruction d (CreateRect width height) = do
  _ <- sequence $ lightDot d <$> inds
  return d
    where inds = indicesForRect (width, height) (dimensions d)
processInstruction d (RotateRow row c) = do
  state <- currentRow d row
  shiftRight d state c
    where currentRow :: DisplayState s -> Int -> ST s (Array Int Bool)
          currentRow (DS cont (dispW, _)) r = do
            row' <- sequence $ readArray cont <$> [ i + r * dispW | i <- [0 .. dispW - 1] ]
            return $ listArray (0, dispW) row'
          shiftRight :: DisplayState s -> Array Int Bool -> Int -> ST s (DisplayState s)
          shiftRight ds@(DS _ (dispW, _)) a s = do
            state <- sequence $ updateBit ds a s <$> [0..dispW-1]
            return $ last state
          updateBit :: DisplayState s -> Array Int Bool -> Int -> Int -> ST s (DisplayState s)
          updateBit ds@(DS cont (dispW, _)) a s i = do
            writeArray cont (row * dispW + i) (a ! wrap dispW (i - s))
            return ds
processInstruction d (RotateColumn col c) = do
  state <- currentCol d col
  shiftDown d state c
    where currentCol :: DisplayState s -> Int -> ST s (Array Int Bool)
          currentCol (DS cont (dispW, dispH)) cl = do
            col' <- sequence $ readArray cont <$> [ i * dispW + cl | i <- [0 .. dispH - 1] ]
            return $ listArray (0, dispH) col'
          shiftDown :: DisplayState s -> Array Int Bool -> Int -> ST s (DisplayState s)
          shiftDown ds@(DS _ (_, dispH)) a s = do
            state <- sequence $ updateBit ds a s <$> [0..dispH-1]
            return $ last state
          updateBit :: DisplayState s -> Array Int Bool -> Int -> Int -> ST s (DisplayState s)
          updateBit ds@(DS cont (dispW, dispH)) a s i = do
            writeArray cont (col + i * dispW) (a ! wrap dispH (i - s))
            return ds

-- | Lights dot on the given position
lightDot :: DisplayState s -> Int -> ST s (DisplayState s)
lightDot d' i = do
  writeArray (contents d') i True
  return d'
  
-- | Returns index wrapped up to 
wrap :: Int -> Int -> Int
wrap m i = if i >= 0 then i else m - (abs i `mod` m)

-- | Calculates indices for given dimensions of rect and display
indicesForRect :: Dimensions -> Dimensions -> [Int] 
indicesForRect (rectW, rectH) (dispW, _) = concatMap generate [0..rectH - 1]
  where generate mult = (+ (mult * dispW)) <$> [0..rectW - 1]

-- | Collects state mapped from elements of the array
collectState :: (Bool -> a) -> DisplayState s -> ST s [[a]]
collectState fun (DS cont (dispW, dispH)) = sequence $ goLine fun cont <$> [0 .. dispH - 1]
  where goLine :: (Bool -> a) -> STArray s Int Bool -> Int -> ST s [a] 
        goLine f a l = sequence $ goBit f a l <$> [0 .. dispW - 1]
        goBit :: (Bool -> a) -> STArray s Int Bool -> Int -> Int -> ST s a
        goBit f a l i = do
          bit <- readArray a (i + l * dispW)
          return $ f bit

-- | Stateful action used to display state (actually ST needed to access array
--   without freezing it.
showState :: DisplayState s -> ST s [String]
showState = collectState (\b -> if b then '#' else '.') 

-- | Count setup bits in the output
countBits :: DisplayState s -> ST s Int
countBits ds = do
  e <- collectState (\b -> if b then 1 else 0) ds
  return $ sum $ sum <$> e

-- | Runs program and returns whatever action supplied returns
runProgram :: (DisplayState s -> ST s a) -> Dimensions -> [Instruction] -> ST s a
runProgram f dims instructions = do
  state <- createDisplay dims
  state' <- sequence $ processInstruction state <$> instructions
  f (last state')
