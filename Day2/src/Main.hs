module Main where

import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = putStrLn "Hello, Haskell!"

data Direction = U | D | L | R
  deriving (Show, Eq, Read)

type Position = (Int, Int)

type Digit = Int

move :: Position -> Direction -> Position
move (x, y) U = (x, max 1 $ y - 1)
move (x, y) D = (x, min 3 $ y + 1)
move (x, y) R = (min 3 $ x + 1, y)
move (x, y) L = (max 1 $ x - 1, y)

positionToDigit :: Position -> Digit
positionToDigit (x, y) = (y - 1) * 3 + x

digitToPosition :: Digit -> Position
digitToPosition d = let y = 1 + ((d - 1) `div` 3)
                    in (d - (y - 1) * 3, y)

readDirections :: String -> Maybe [Direction]
readDirections str = sequence $ readDir <$> str
  where readDir 'U' = Just U
        readDir 'D' = Just D
        readDir 'R' = Just R
        readDir 'L' = Just L
        readDir _   = Nothing

readInput :: FilePath -> IO (Maybe [[Direction]])
readInput path = do
  contents <- (T.unpack <$>) <$> T.lines <$> TIO.readFile path
  return $ sequence $ readDirections <$> contents

go :: Digit -> [Direction] -> Digit
go d dirs = positionToDigit $ foldl' move (digitToPosition d) dirs

findCode :: Digit -> [[Direction]] -> [Digit]
findCode d dirs = reverse . snd $ foldl' getCode (d, []) dirs

getCode :: (Digit, [Digit]) -> [Direction] -> (Digit, [Digit])
getCode (d, dgts) dirs = let d' = go d dirs
                         in (d', d':dgts)


positionValid :: Position -> Bool
positionValid (x, y) = abs x + abs y <= 2

move2 :: Position -> Direction -> Position
move2 pos dir = let pos' = step pos dir
                in if positionValid pos'
                   then pos'
                   else pos
  where step :: Position -> Direction -> Position
        step (x, y) U = (x, y + 1)
        step (x, y) D = (x, y - 1)
        step (x, y) R = (x + 1, y)
        step (x, y) L = (x - 1, y)


positionToDigit2 :: Position -> Digit
positionToDigit2 (0, 0) = 7
positionToDigit2 (-1, 0) = 6
positionToDigit2 (-2, 0) = 5
positionToDigit2 (1, 0) = 8
positionToDigit2 (2, 0) = 9
positionToDigit2 (-1, 1) = 2
positionToDigit2 (0, 1) = 3
positionToDigit2 (1, 1) = 4
positionToDigit2 (0, 2) = 1
positionToDigit2 (-1, -1) = 10
positionToDigit2 (0, -1) = 11
positionToDigit2 (1, -1) =12
positionToDigit2 (0, -2) = 13
positionToDigit2 _ = undefined

go2 :: Position -> [Direction] -> Position
go2 = foldl' move2

getCode2 :: (Position, [Position]) -> [Direction] -> (Position, [Position])
getCode2 (d, dgts) dirs = let d' = go2 d dirs
                          in (d', d':dgts)

findCode2 :: Position -> [[Direction]] -> [Position]
findCode2 p dirs = reverse . snd $ foldl' getCode2 (p, []) dirs


-- readDirection :: Char -> Maybe (Direction, T.Text)
-- readDirection text = do
--   (char, rest) <- T.uncons text
--   case char of
--     'U' -> Just (U, rest)
--     'D' -> Just (D, rest)
--     'R' -> Just (R, rest)
--     'L' -> Just (L, rest)
--     _   -> Nothing
--
-- readDirections :: T.Text -> Maybe ([Direction], T.Text)
