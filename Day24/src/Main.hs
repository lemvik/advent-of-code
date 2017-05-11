module Main where

import Common.AStar
import Definitions
import Parser
import Data.Maybe (fromJust)
import qualified Data.Text.IO as TIO
import Data.List (permutations, minimumBy)
import Data.Ord (comparing)
import System.Environment (getArgs)

-- | Finds all neighbours of given position
neighbours :: Labyrinth -> Position -> [(Position, Distance)]
neighbours l p = addDist <$> accessible l p
  where addDist (x, y, _) = ((x, y), 1)

-- | Heuristic that estimates Manhattan distance to target.
heuristic :: Position -> Position -> Distance
heuristic (xs, ys) (xf, yf) = fromIntegral $ abs (xs - xf) + abs (ys - yf)

-- | Finds shortest path from starting position to final position
findPath :: Labyrinth -> Position -> Position -> Maybe [Position]
findPath l pS pF = aStar Shortest (neighbours l) (heuristic pS) (== pF) pS

-- | Finds shortest distance between two given positions in labyrinth
findDistance :: Labyrinth -> Position -> Position -> Maybe Distance
findDistance l pS pF = fromIntegral . (\x -> x - 1) . length <$> findPath l pS pF

-- | Finds shortest distance from one unique position to another
fromAtoB :: Labyrinth -> CellType -> CellType -> Maybe Distance
fromAtoB l cA cB = do
  pA <- findNode l cA
  pB <- findNode l cB
  findDistance l pA pB

-- | Returns all pairs of elements that could be constructed from given (unordered)
allPairs :: [a] -> [(a, a)]
allPairs []     = []
allPairs [_]    = []
allPairs (x:xs) = zip (repeat x) xs ++ allPairs xs

-- | Finds all distances between positions in labyrinth
distancesBetween :: Labyrinth -> [CellType] -> Maybe [(CellType, CellType, Distance)]
distancesBetween l tps = sequence $ distance <$> allPairs tps
  where distance (cA, cB) = (\d -> (cA, cB, d)) <$> fromAtoB l cA cB

-- | Finds minimal distance in all permutations
commersantBruteForce :: Distances -> [CellType] -> Distance
commersantBruteForce dsts cls = snd $ minimumBy (comparing snd) $ allPaths dsts cls

-- | Finds all paths and theirs distances
allPaths :: Distances -> [CellType] -> [([CellType], Distance)]
allPaths dsts (s:cls) = computeDist . wrap <$> permutations cls
  where wrap ls = (s:ls) ++ [s]
        computeDist lst = (lst, go lst 0)
        go [] acc = acc
        go [_] acc = acc
        go (x:y:rest) acc = go (y:rest) (acc + fromJust (getDistance dsts x y))
allPaths _ []         = []

main :: IO ()
main = do
  [p] <- getArgs
  content <- TIO.readFile p
  print $ do
    lab <- readLabyrinth content
    dists <- distancesBetween lab [0..7]
    return $ commersantBruteForce (distancesFromList dists) [0..7]
