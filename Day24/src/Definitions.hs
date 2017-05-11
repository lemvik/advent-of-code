module Definitions where

import Common.AStar
import Data.Array.Unboxed
import Data.List (find, foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

-- | Type alias for cells - as they are stores in array, they should be ints
type CellType = Int

-- | Alias for position
type Position = (Int, Int)

-- | Path and wall are represented as negative values
path, wall :: CellType
path = -1 
wall = -2

-- | Labyrinth is a two-dimensional array of cells
type Labyrinth = UArray Position CellType

-- | Accessor for cells
cellType :: Labyrinth -> Position -> CellType
cellType l i = l ! i

-- | Finds node in the labyrinth
findNode :: Labyrinth -> CellType -> Maybe Position
findNode l t = find matching (range . bounds $ l)
  where matching pos = cellType l pos == t

-- | Cell is coordinates + type
type Cell = (Int, Int, CellType)

-- | Marks position as a cell of given type
markCell :: Position -> CellType -> Cell
markCell (x, y) c = (x, y, c)

-- | Returns list of elements around given position
around :: Labyrinth -> Position -> [Cell]
around l p@(x, y) = annot <$> filter valid ar
  where bnds = bounds l
        ar = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
        valid p' = (p' /= p) && inRange bnds p'
        annot p'@(x', y') = (x', y', cellType l p')

-- | Returns list of accesible elements around given one.
accessible :: Labyrinth -> Position -> [Cell] 
accessible l p = filter (\(_, _, t) -> t /= wall) $ around l p


-- | Map of distances between interesting cells 
type Distances = Map CellType (Map CellType Distance)

-- | Finds distance between two nodes
getDistance :: Distances -> CellType -> CellType -> Maybe Distance 
getDistance dst cA cB = M.lookup cA dst >>= M.lookup cB

-- | Creates a map of distances from given list
distancesFromList :: [(CellType, CellType, Distance)] -> Distances
distancesFromList = foldl' add M.empty
  where add topMap (cA, cB, d) = let topMap' = M.alter (add' cB d) cA topMap
                                     topMap'' = M.alter (add' cA d) cB topMap'
                                 in topMap''
        add' c d (Just subMap) = Just $ M.insert c d subMap
        add' c d Nothing = Just $ M.singleton c d 
