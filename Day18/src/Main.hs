module Main where

import Data.List (unfoldr)
import Data.Array.IArray
import Data.Foldable (foldl')
import System.Environment (getArgs)

-- | Tile is either trap or is safe
data Tile = Safe | Trap
          deriving (Eq)

instance Show Tile where
  show Safe = "."
  show Trap = "^"

-- | Tile line is an array of tiles
newtype TileLine = TileLine (Array Int Tile)
                 deriving (Eq)

instance Show TileLine where
  show (TileLine ln) = foldl' (\r t -> r ++ show t) "" ln

-- | Function that attempts to read the floor from input.
readTileLine :: String -> Maybe TileLine
readTileLine str = construct <$> sequence (readTile <$> str)
  where readTile '.' = Just Safe
        readTile '^' = Just Trap
        readTile _   = Nothing
        construct ln = TileLine (listArray (0, length ln - 1) ln)

-- | The rule for trap generation
rule :: (Tile, Tile, Tile) -> Tile
rule (Trap, Trap, Safe) = Trap
rule (Safe, Trap, Trap) = Trap
rule (Trap, Safe, Safe) = Trap
rule (Safe, Safe, Trap) = Trap
rule _                  = Safe

-- | Generates next tile line based on current one (assumes floor is at least two tiles wide)
nextTileLine :: TileLine -> TileLine
nextTileLine (TileLine ln) = TileLine $ listArray (l,u) line
  where (l,u) = bounds ln
        line = rule . neighbours <$> [l..u]
        neighbours i | i == l = (Safe, ln ! i, ln ! (i + 1))
                     | i == u = (ln ! (i - 1), ln ! i, Safe)
                     | otherwise = (ln ! (i - 1), ln ! i, ln ! (i + 1))

-- | Generate N lines starting with given one (including it)
generateLines :: Int -> TileLine -> [TileLine]
generateLines nu start = take nu $ unfoldr generate start
  where generate ln = Just (ln, nextTileLine ln)

-- | Counts number of safe tiles
countSafe :: TileLine -> Int
countSafe (TileLine ln) = foldl' add 0 ln
  where add r Safe = r + 1
        add r Trap = r

main :: IO ()
main = do
  [n, tiles] <- getArgs
  print $ do
    start <- readTileLine tiles
    return $ sum $ countSafe <$> generateLines (read n) start
