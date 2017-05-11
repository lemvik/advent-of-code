module Main where

import Definitions
import Parser
import Text.Printf
import qualified Data.Text.IO as IO
import qualified Data.Text as T
import System.Environment (getArgs)
import Data.Ord (comparing)
import Data.List (maximumBy, transpose, groupBy, intercalate)
import Data.Foldable (foldl')

-- | Checks if disk is empty
isEmpty :: Disk -> Bool
isEmpty (Disk _ _ 0 _) = True
isEmpty _              = False

-- | Counts possibilities for transfer
viableNodesCount :: [Disk] -> Int
viableNodesCount dsks = foldl' viable 0 (filter (not . isEmpty) dsks)
  where viable s d = s + length (filter (enoughFor d) dsks)
        enoughFor d d' = d /= d' && available d' >= used d

-- | Calculate steps to move:
--   1. How many steps does it take to move emptiness to goal (near the goal) - (30, 29)
--   2. One to transfer goal to empty space
--   3. 5 x 30 (each shift to the right takes us 5 moves - one to move the goal and 4 to move the empty space)
stepsToMove :: [Disk] -> Int
stepsToMove = undefined 

displayField :: [Disk] -> [String]
displayField dsks = displayLine <$> sorted
  where sorted = transpose $ groupBy (\a b -> (fst . location $ a) == (fst . location $ b)) dsks
        displayLine = foldl' displayD ""
        displayD s d = s ++ printf "%-4d " (used d)

stats entries = (filter isEmpty entries,
                 used <$> filter twoRows entries,
                 filter twoRows' entries,
                 location <$> filter large entries,
                 location $ maximumBy (comparing (fst . location)) entries)
  where twoRows (Disk (_, y) _ _ _) = y < 2
        twoRows' (Disk (_, y) _ u _) = y < 2 && u > 86
        large (Disk _ _ u _) = u > 86

main :: IO ()
main = do
  [path] <- getArgs
  contents <- IO.readFile path
  case readEntries . drop 2 $ T.lines contents of
    Left e -> print e
    Right entries -> print $ stats entries  -- sequence_ $ putStrLn <$> displayField entries
      
--    return (filter isEmpty entries,
--            used <$> filter twoRows entries,
--            filter twoRows' entries,
--            location <$> filter large entries,
--            location $ maximumBy (comparing (fst . location)) entries)
--      where twoRows (Disk (_, y) _ _ _) = y < 2
--            twoRows' (Disk (_, y) _ u _) = y < 2 && u > 86
--            large (Disk _ _ u _) = u > 86
