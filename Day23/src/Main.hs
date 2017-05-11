module Main where

import Parse
import Machine
import Text.Parsec
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Array.IArray
import System.Environment (getArgs)

-- | Creates program from instructions
readProgram :: Text -> Either ParseError Program
readProgram txt = do
  instructions <- parseInstructions txt
  return $ listArray (0, length instructions - 1) instructions

main :: IO ()
main = do
  [path] <- getArgs
  contents <- TIO.readFile path
  print $ do
    prog <- readProgram contents
    return $ runImage (MachineImage 0 12 0 0 0 prog)
