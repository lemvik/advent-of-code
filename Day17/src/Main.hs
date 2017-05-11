module Main where

import Data.Ord (comparing)
import Data.Foldable (maximumBy)
import Common.AStar
import Common.Depth
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Maybe (catMaybes, mapMaybe, fromJust)
import qualified Data.List as L
import System.Environment (getArgs)

-- | Coordinate of the element
type Coord = Word

-- | Possible steps to take
data Step = U | D | L | R
          deriving (Eq, Ord, Show)

-- | List of all directions
allDirections :: [Step]
allDirections = [U, D, L, R] -- Note the ordering - it is used in hashMoves function

-- | Path taken so far 
data Path = Path { coords :: (Coord, Coord)
                 , steps :: [Step]
                 , allowed :: [Step] }
          deriving (Eq, Show)

instance Ord Path where
  compare (Path ca sa aa) (Path cb sb ab) = let c = compare ca cb
                                            in case c of
                                                 EQ -> let a = compare aa ab
                                                       in case a of
                                                            EQ -> compare (length sb) (length sa) -- Notice the reverse
                                                            _  -> a
                                                 _  -> c

-- | Allowed moves from given position
moves :: String -> Path -> [Path]
moves salt p@(Path _ _ als) = catMaybes $ move salt p <$> als

-- | Attempts to add given step to path
move :: String -> Path -> Step -> Maybe Path
move salt (Path crds sts als) st = do
  (crds', viables) <- step crds st
  if st `elem` als
    then return $ let sts' = st:sts
                      hashAllows = hashMoves salt (reverse sts')
                      als' = L.intersect viables hashAllows
                  in Path crds' sts' als'
    else Nothing

-- | Try to do a single step from given coordinate into given direction
step :: (Coord, Coord) -> Step -> Maybe ((Coord, Coord), [Step])
step (x, y) U | y > 0 = let c = (x, y - 1) in Just (c, allowedSteps c)
step (x, y) D | y < 3 = let c = (x, y + 1) in Just (c, allowedSteps c)
step (x, y) L | x > 0 = let c = (x - 1, y) in Just (c, allowedSteps c)
step (x, y) R | x < 3 = let c = (x + 1, y) in Just (c, allowedSteps c)
step _      _         = Nothing

-- | Steps available in given coordinate.
allowedSteps :: (Coord, Coord) -> [Step]
allowedSteps (x, y) = mapMaybe go allDirections
  where go L | x > 0 = Just L
        go R | x < 3 = Just R
        go D | y < 3 = Just D
        go U | y > 0 = Just U
        go _         = Nothing 

-- | Letters signifying the door is open
allowedLetters :: String 
allowedLetters = ['b'..'f']

-- | Computes moves allowed by hashing rule - if first 4 letters of hash are not digits or a - move is allowed
hashMoves :: String -> [Step] -> [Step]
hashMoves str sts = let path = BC.pack $ str ++ concatMap show sts
                        res = show $ md5 path
                    in mapMaybe check $ zip allDirections (take 4 res)
  where check (d, letter) = if letter `elem` allowedLetters
                            then Just d
                            else Nothing

-- | Heuristic to use - distance from bottom-left corner
heuristic :: Path -> Distance 
heuristic (Path (x, y) _ _) = abs (3 - x) + abs (3 - y)

-- | Functions that generates neighbours with attached distance
movesDist :: String -> Path -> [(Path, Distance)]
movesDist salt path = (\p -> (p, 1)) <$> moves salt path

-- | We should stop when we reach bottom corner
stopper :: Path -> Bool
stopper (Path (x, y) _ _) = x == 3 && y == 3

-- | Initial position - top left corner
initialPosition :: String -> Path
initialPosition salt = let pos = (0, 0)
                           als' = L.intersect (hashMoves salt []) (allowedSteps pos)
                       in Path pos [] als'

testStepper :: String -> Step -> Path -> Path
testStepper salt s p = fromJust $ move salt p s

-- | Performs search with given salt
search :: SearchType -> String -> Maybe String
search t salt = (concatMap show . reverse . steps . last) <$> aStar t (movesDist salt) heuristic stopper (initialPosition salt)

searchLongest :: String -> Maybe String
searchLongest salt = (concatMap show . reverse . steps) <$> depthSearch srch (initialPosition salt)
  where srch = Search { searchBrancher = brancher  
                      , searchMatcher  = matcher 
                      , searchSelector = lengthMaximizer
                      , searchTerminator = const False
                      }
        lengthMaximizer pa pb = maximumBy (comparing (length . steps)) [pa, pb]
        matcher (Path (x, y) _ _) = x == 3 && y == 3 
        brancher p = if matcher p
                     then []
                     else moves salt p

main :: IO ()
main = do
  [salt] <- getArgs
  print $ length <$> searchLongest salt
