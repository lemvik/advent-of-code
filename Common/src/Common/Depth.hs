module Common.Depth where

import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M

-- | Type for brancher function
type Brancher a = a -> [a]

-- | Selector function - must select better element from two possibilities
--   TODO: might be worth it to replace it with Monoid constraint
type Selector a = a -> a -> a

-- | Matcher checks if element matches requirements
type Matcher a = a -> Bool

-- | When this function returns true - search stops. Only called when Matcher returned true.
type Terminator a = a -> Bool

-- | Search setup
data Search a = Search { searchBrancher   :: Brancher a
                       , searchSelector   :: Selector a
                       , searchMatcher    :: Matcher a
                       , searchTerminator :: Terminator a }

-- | Performs a depth search on the graph. Monoid instance is required to be able
--   to combine all instances that matcher marks
depthSearch :: Search a -> a -> Maybe a
depthSearch (Search brancher selector matcher terminator) start = go [start] Nothing
  where go []        acc = acc
        go (el:rest) acc = if matcher el 
                           then if terminator el
                                then select acc el
                                else go (brancher el ++ rest) (select acc el)
                           else go (brancher el ++ rest) acc
        select Nothing e  = Just e
        select (Just r) e = Just (selector r e)


testSearch :: Search Int
testSearch = let graph = M.fromList [(1, [2,3]), (2, [4]), (3, [5, 6])]
             in Search { searchBrancher = \e -> fromMaybe [] (M.lookup e graph)
                       , searchSelector = max 
                       , searchMatcher  = odd
                       , searchTerminator = const False
                       }
