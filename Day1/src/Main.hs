module Main where

import System.Environment

import Data.List (foldl')
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

main :: IO ()
main = do
    args <- getArgs
    input <- readInput $ head args
    print (findLast input, findRepeating input)

findLast :: Either String [Move] -> Either String Position
findLast input = do
  instructions <- input
  return $ follow instructions (North, 0, 0)

findRepeating :: Either String [Move] -> Either String Position
findRepeating input = do
  instructions <- input
  maybeToEither "Unable to find repeating position" $ findFirst instructions (North, 0, 0)

data Direction = North | East | West | South
               deriving (Show, Eq, Ord)

data Turn = TurnRight | TurnLeft
    deriving (Show)

data Move = Move Turn Int
          deriving (Show)

type Position = (Direction, Int, Int)

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither = flip maybe Right . Left

readTurn :: Char -> Either String Turn
readTurn 'R' = Right TurnRight
readTurn 'L' = Right TurnLeft
readTurn _   = Left "Unable to read turn"

readMove :: T.Text -> Either String Move
readMove text = do
    (letter, rest) <- maybeToEither "Unable to read direction" (T.uncons text)
    (count, _)     <- TR.decimal rest
    turn           <- readTurn letter
    return $ Move turn count

readInput :: FilePath -> IO (Either String [Move])
readInput path = do
  contents <- T.words <$> TIO.readFile path
  return $ sequence $ readMove . T.dropWhileEnd (== ',') <$> contents

follow :: [Move] -> Position -> Position
follow moves pos = foldl' step pos moves

step :: Position -> Move -> Position
step (dir, x, y) (Move t len) = let (dir', (x', y')) = delta dir t len
                                             in (dir', x + x', y + y')
  where delta :: Direction -> Turn -> Int -> (Direction, (Int, Int)) 
        delta dir t len = let dir' = turn dir t
                          in (dir', case dir' of
                                      North -> (0, len)
                                      South -> (0, -len)
                                      West  -> (-len, 0)
                                      East  -> (len, 0))
        turn :: Direction -> Turn -> Direction
        turn North TurnLeft  = West
        turn North TurnRight = East 
        turn South TurnLeft  = East 
        turn South TurnRight = West 
        turn East TurnLeft  = North 
        turn East TurnRight = South 
        turn West TurnLeft  = South 
        turn West TurnRight = North 

findFirst :: [Move] -> Position -> Maybe Position
findFirst moves pos@(_, x, y) = run moves pos (S.singleton (x, y))
  where run :: [Move] -> Position -> S.Set (Int, Int) -> Maybe Position
        run []     _   _   = Nothing
        run (x:xs) pos set = let pos'@(_, x', y') = step pos x
                                 breadcrumbs      = enumerate pos pos'
                                 (visited, set')  = check set breadcrumbs
                             in case visited of
                                  res@(Just _) -> res
                                  Nothing      -> run xs pos' set'
          where check :: S.Set (Int, Int) -> [Position] -> (Maybe Position, S.Set (Int, Int))
                check set []     = (Nothing, set)
                check set (x@(_, a, b):xs) = if S.member (a, b) set then (Just x, set) else check (S.insert (a, b) set) xs

                enumerate :: Position -> Position -> [Position]
                enumerate (_, x, y) (d', x', y') = tail $ if x == x'
                                                         then if y < y'
                                                              then take (y' - y) $ iterate (\(d, a, b) -> (d, a, b + 1)) (d', x, y)
                                                              else take (y - y') $ iterate (\(d, a, b) -> (d, a, b - 1)) (d', x, y)
                                                         else if x < x'
                                                              then take (x' - x) $ iterate (\(d, a, b) -> (d, a + 1, b)) (d', x, y)
                                                              else take (x - x') $ iterate (\(d, a, b) -> (d, a - 1, b)) (d', x, y)
