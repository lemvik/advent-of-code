module Main where

import Data.Ord (comparing)
import Control.Monad (join)
import Data.Maybe (maybe, fromMaybe, listToMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as PM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Bits (popCount)
import Data.Foldable (foldl', maximumBy)
import System.Environment (getArgs)

main :: IO ()

main = do
  [dist] <- (read <$>) <$> getArgs
  let vis = aProbe (1, 1) dist
  print (S.size vis, maximumBy (comparing fst) $ S.toList vis, maximumBy (comparing snd) $ S.toList vis)
  putStrLn $ display 25 vis
--  print $ length <$> aStar (defaultHeuristic (31, 39)) (1, 1) (31, 39) 

-- | Each coordinate is an unsigned integer
type Coord = Word

-- | Position is represented as pair of coordinates
type Position = (Coord, Coord)

-- | Element on the floor is either wall or space
data Element = Wall | Space
             deriving (Show, Eq)

-- | Maze designer's favorite number
magicNumber :: Word
--magicNumber = 10
magicNumber = 1358

-- | Computes what element is at given position
elementAt :: Position -> Element
elementAt (x, y) = let n = x * x + 3 * x + 2 * x * y + y + y * y + magicNumber
                   in if odd . popCount $ n
                      then Wall
                      else Space


-- | Type alias for weight
type Weight = Word

-- | Weighted position - a position with associated weight
newtype WeightedPosition = WPosition (Coord, Coord, Weight)
                         deriving (Show, Eq)

instance Ord WeightedPosition where
  compare (WPosition (_, _, wa)) (WPosition (_, _, wb)) = compare wa wb

-- | Assign a weight to position
weightPosition :: Weight -> Position -> WeightedPosition
weightPosition w (x, y) = WPosition (x, y, w)

-- | Retrieve coordinates from weighted position
getPosition :: WeightedPosition -> Position
getPosition (WPosition (x, y, _)) = (x, y)

-- | Retrieves weight from weighted position
getWeight :: WeightedPosition -> Weight
getWeight (WPosition (_, _, w)) = w

-- | Converts weighted position to a map-friendly tuple
toTuple :: WeightedPosition -> (Position, Weight)
toTuple pos = (getPosition pos, getWeight pos)

-- | Distance type alias
type Distance = Word

-- | Type representing iteration state
data IterationState = IS { aQueue :: MinQueue WeightedPosition
                         , openSet :: Set Position
                         , closedSet :: Set Position
                         , backRefs :: Map Position Position
                         , distances :: Map Position Distance
                         }

-- | Initialize iteration state for moving from given position
initialState :: Position -> IterationState
initialState pos = IS { aQueue = PM.singleton $ weightPosition 0 pos
                      , openSet = S.singleton pos
                      , closedSet = S.empty
                      , backRefs = M.empty
                      , distances = M.singleton pos 0
                      }

-- | Retrieves next position to move to
nextStep :: IterationState -> Maybe Position
nextStep st = getPosition <$> (PM.getMin . aQueue $ st)

-- | Gets all positions around given one.
around :: Position -> [Position]
around p = let (x', y') = convert p
           in convert <$> filter valid ([(x' - 1, y'), (x'+1, y'), (x', y'-1), (x', y'+1)] :: [(Int, Int)])
  where convert (a, b) = (fromIntegral a, fromIntegral b)
        valid (a, b) = a >= 0 && b >= 0

-- | Computes nearby spaces for given position (only considers up-down-left-right neighbours)
nearSpaces :: Position -> [Position]
nearSpaces pos = fst <$> filter aSpace (label <$> around pos)
  where label p = (p, elementAt p)
        aSpace (_, Space) = True
        aSpace _          = False

-- | Checks if this position has been already visited
visited :: IterationState -> Position -> Bool
visited state pos = S.member pos (closedSet state) 

-- | Returns true if given position is to be considered yet (i.e. present in open set)
pending :: IterationState -> Position -> Bool
pending state pos = S.member pos (openSet state)

-- | Retrieves computed distance to given position if it was indeed computed before
distanceTo :: IterationState -> Position -> Maybe Distance
distanceTo state pos = M.lookup pos (distances state)

-- | Returns a list of all positions around given one
--   that are feasible to be considered - that is, not visited yet,
--   yield shorter path 
considerables :: IterationState -> Position -> Maybe [WeightedPosition]
considerables state pos = do
  dist <- distanceTo state pos
  return $ let dn = weightPosition (dist + 1) <$> filter (not . visited state) (nearSpaces pos)
           in filter unseen dn 
  where unseen wpos = not (pending state $ getPosition wpos) 

-- | A* algorithm for finding shortest path from given position to target position using
--   given heuristic
aStar :: (Position -> Word) -> Position -> Position -> Maybe [Position]
aStar heur start end = join $ reconstructPath <$> iter (initialState start)
  where iter :: IterationState -> Maybe IterationState -- Finds path or returns Nothing if openSet is exhausted.
        iter state = do
          next <- nextStep state
          if next == end
            then return state
            else iter $ proceed state next

        proceed :: IterationState -> Position -> IterationState -- Performs updates on the iteration state after moving to given position
        proceed state pos = let con = fromMaybe [] $ considerables state pos
                            in foldl' (addNeighbour pos) (removePos pos state) con 
          where removePos :: Position -> IterationState -> IterationState
                removePos p st@(IS qu os cs _ _) = st { aQueue = PM.deleteMin qu
                                                      , openSet = S.delete p os
                                                      , closedSet = S.insert p cs
                                                      }
                addNeighbour :: Position -> IterationState -> WeightedPosition -> IterationState
                addNeighbour p (IS qu os cs brfs dists) (WPosition (x, y, w)) = let neigh = (x, y)
                                                                                    wneigh = weightPosition (w + heur neigh) neigh
                                                                                    qu' = PM.insert wneigh qu
                                                                                    os' = S.insert neigh os
                                                                                    brfs' = M.insert neigh p brfs
                                                                                    dists' = M.insert neigh w dists
                                                                                in IS qu' os' cs brfs' dists'
        reconstructPath :: IterationState -> Maybe [Position] -- Reconstructs path from given iteration state.
        reconstructPath (IS _ _ _ brfs _) = go end [end]
          where go pos acc | pos == start = Just acc
                           | otherwise    = do
                               pr <- M.lookup pos brfs
                               go pr (pr:acc)

-- | Probes all positions below given distance (in actual steps, not euclidean) and returns set of all
--   visited positions.
aProbe :: Position -> Distance -> Set Position 
aProbe pos dist = closedSet $ iter (initialState pos)
  where iter :: IterationState -> IterationState
        iter st = maybe st (step st) (next st)

        next :: IterationState -> Maybe (Position, [WeightedPosition])
        next st@(IS _ os _ _ _) = do
          n <- listToMaybe (S.toList os) -- We make sure that open set only contains positions below given distance
          neighs' <- considerables st n
          return (n, neighs')

        step :: IterationState -> (Position, [WeightedPosition]) -> IterationState
        step st@(IS _ os cs _ dists) (p, neighs) = let ns = filter ((<= dist) . getWeight) neighs
                                                       ns'  = getPosition <$> ns
                                                       ns'' = toTuple <$> ns
                                                       os' = S.union (S.delete p os) (S.fromList ns')
                                                       cs' = S.insert p cs
                                                       dists' = M.union (M.fromList ns'') dists
                                                in iter $ st { openSet = os'
                                                             , closedSet = cs'
                                                             , distances = dists'
                                                             }

-- | Default heuristic just uses Manhattan distance between given position and target
defaultHeuristic :: Position -> Position -> Word
defaultHeuristic (tx, ty) (x, y) = abs (tx - x) + abs (ty - y)

-- | Displays maze with O for visited locations
display :: Distance -> Set Position -> String
display limit vis = unlines $ displayLine <$> [0..limit]
  where displayLine line = displayElement line <$> [0..limit]
        displayElement l c = case elementAt (l, c) of
          Wall -> if S.member (l, c) vis
                  then undefined
                  else '#'
          Space -> if S.member (l, c) vis
                   then 'O'
                   else '.'
