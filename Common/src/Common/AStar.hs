{-# LANGUAGE FlexibleContexts #-}
module Common.AStar where

import Control.Monad (join)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as PMin
import Data.PQueue.Max (MaxQueue)
import qualified Data.PQueue.Max as PMax
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Foldable (foldl')

-- | Distance between two nodes
type Distance = Word 

-- | Type for weight (non-negative)
type Weight = Word

-- | Weight wrapper around element
data Weighted a = Weighted { element :: a
                           , weight :: Weight }

-- | When we are considering equality, compare both weight and element
instance (Eq a) => Eq (Weighted a) where
  (Weighted a wa) == (Weighted b wb) = a == b && wa == wb

-- | Weighted elements are compared using their weight
instance (Ord a) => Ord (Weighted a) where
  compare (Weighted ea a) (Weighted eb b) = let c = compare a b
                                            in case c of
                                                 EQ -> compare ea eb
                                                 _  -> c

-- | Wrapper allowing selecting the priority queue
data Queue a = MinQ { minQueue :: MinQueue a }
             | MaxQ { maxQueue :: MaxQueue a }

-- | Returns next element from the queue
nextElement :: Queue a -> Maybe a
nextElement (MinQ q) = PMin.getMin q
nextElement (MaxQ q) = PMax.getMax q

-- | Deletes next element from the queue
deleteNext :: (Ord a) => Queue a -> Queue a
deleteNext (MinQ q) = MinQ $ PMin.deleteMin q
deleteNext (MaxQ q) = MaxQ $ PMax.deleteMax q

-- | Inserts element in to queue
insertNext :: (Ord a) => a -> Queue a -> Queue a
insertNext el (MinQ q) = MinQ $ PMin.insert el q
insertNext el (MaxQ q) = MaxQ $ PMax.insert el q

-- | Type representing search state
data SearchState a = SearchState { aQueue :: Queue (Weighted a)
                                 , openSet :: Set a
                                 , closedSet :: Set a
                                 , backRefs :: Map a a 
                                 , distances :: Map a Distance }

-- | What type of path do we search for
data SearchType = Shortest
                | Longest

-- | Returns appropriate queue for search
queueForSearch :: SearchType -> a -> Queue a
queueForSearch Shortest el = MinQ $ PMin.singleton el 
queueForSearch Longest el = MaxQ $ PMax.singleton el

-- | Constructs initial search state
initialState :: SearchType -> a -> SearchState a
initialState t el = SearchState { aQueue = queueForSearch t $ Weighted el 0
                                , openSet = S.singleton el 
                                , closedSet = S.empty
                                , backRefs = M.empty
                                , distances = M.singleton el 0 }

-- | Retrieves next position to move to
nextStep :: SearchState a -> Maybe a
nextStep st = element <$> (nextElement . aQueue $ st)

-- | Checks if this position has been already visited
visited :: (Ord a) => SearchState a -> a -> Bool
visited state pos = S.member pos (closedSet state) 

-- | Returns true if given position is to be considered yet (i.e. present in open set)
pending :: (Ord a) => SearchState a -> a -> Bool
pending state pos = S.member pos (openSet state)

-- | Retrieves computed distance to given position if it was indeed computed before
distanceTo :: (Ord a) => SearchState a -> a -> Maybe Distance
distanceTo state pos = M.lookup pos (distances state)
  
-- | Returns a list of all positions around given one
--   that are feasible to be considered - that is, not visited yet,
--   yield shorter path 
considerables :: (Ord a) => (a -> [(a, Distance)]) -> SearchState a -> a -> [Weighted a]
considerables brancher state el = fromMaybe [] $ do
  dist <- distanceTo state el 
  return $ addDist dist <$> filter (unseen . fst) (brancher el)
  where unseen e = not (pending state e) && not (visited state e)
        addDist d (e, d') = Weighted e (d + d')

-- | Perform A* search for end state from start using given heuristic
aStar :: (Ord a) => SearchType -> (a -> [(a, Distance)]) -> (a -> Distance) -> (a -> Bool) -> a -> Maybe [a]
aStar t brancher heur stop start = join $ uncurry reconstructPath <$> iter (initialState t start)
  where iter state = do -- Finds path or returns Nothing if openSet is exhausted.
          next <- nextStep state
          if stop next 
            then return (state, next)
            else iter $ proceed state next

        --proceed :: (Eq a) => SearchState a -> a -> SearchState a -- Performs updates on the iteration state after moving to given position
        proceed state pos = let con = considerables brancher state pos
                            in foldl' (addNeighbour pos) (removePos pos state) con 
          where removePos p st@(SearchState qu os cs _ _) = st { aQueue = deleteNext qu
                                                               , openSet = S.delete p os
                                                               , closedSet = S.insert p cs }
                addNeighbour p (SearchState qu os cs brfs dists) (Weighted neigh w) = let wneigh = Weighted neigh (w + heur neigh)
                                                                                          qu' = insertNext wneigh qu
                                                                                          os' = S.insert neigh os
                                                                                          brfs' = M.insert neigh p brfs
                                                                                          dists' = M.insert neigh w dists
                                                                                      in SearchState qu' os' cs brfs' dists'
        reconstructPath (SearchState _ _ _ brfs _) end = go end [end] -- Reconstructs path from given iteration state.
          where go pos acc | pos == start = Just acc
                           | otherwise    = do
                               pr <- M.lookup pos brfs
                               go pr (pr:acc)
