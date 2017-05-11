module Main where

import Debug.Trace
import Definitions
import Parser
import Data.Array.IArray
import qualified Data.Text.IO as IO
import System.Environment (getArgs)
import Data.Foldable (find, asum, foldl')
import Data.Maybe (fromMaybe)
import Data.List (permutations)

-- | Alias for string as array of chars - we need to do a lot of indexing
--   thus this alias is handy
type AString = Array Int Char

-- | Returns index of char in array
indexOfChar :: AString -> Char -> Maybe Int
indexOfChar str c = asum $ match <$> range (bounds str)
  where match i = if str ! i == c then Just i else Nothing

-- | Converts list string to array string.
fromString :: String -> AString
fromString str = listArray (0, length str - 1) str

-- | Converts from array string into list string
toString :: AString -> String
toString = elems

-- | Processes a single command
processPermutation :: AString -> Permutation -> AString
processPermutation str (SwapPosition f t) = ixmap (bounds str) swp str
  where swp j | j == f    = t
              | j == t    = f
              | otherwise = j
processPermutation str (SwapLetters a b) = fromMaybe str $ do
  posA <- indexOfChar str a
  posB <- indexOfChar str b
  return $ processPermutation str (SwapPosition posA posB)
processPermutation str (Rotate RotateLeft c) = ixmap bnds rot str
  where bnds = bounds str
        (_, u) = bnds
        c' = c `mod` rangeSize bnds
        rot i | i <= u - c' = i + c'
              | otherwise   = i - (u - c' + 1) 
processPermutation str (Rotate RotateRight c) = ixmap bnds rot str
  where bnds = bounds str
        c' = c `mod` rangeSize bnds 
        rot i | i < c'    = i + (rangeSize bnds - c')
              | otherwise = i - c'
processPermutation str (RotateRelative c) = fromMaybe str $ do
  posC <- indexOfChar str c
  return $ processPermutation str (Rotate RotateRight $ adjust posC)
    where adjust i = 1 + i + (if i >= 4 then 1 else 0)
processPermutation str (Reverse f t) = ixmap (bounds str) rev str
  where rev i | i < f || i > t = i
              | otherwise = f + (t - i)
processPermutation str (Move f t) = ixmap (bounds str) mv str
  where mv i | i == t         = f
             | (f < t && (i < f || i > t)) || (f > t && (i > f || i < t)) = i
             | f < t          = i + 1
             | f > t          = i - 1
             | otherwise      = undefined

-- | Applies a list of permutations to string
applyPermutations :: AString -> [Permutation] -> AString
applyPermutations = foldl' processPermutation

-- | Brute-force the password
crackPassword :: String -> [Permutation] -> Maybe String
crackPassword str perms = find crack (permutations str)
  where crack cand = str == toString (applyPermutations (fromString cand) perms)

main :: IO ()
main = do
  [str, path] <- getArgs
  contents <- IO.readFile path
  print $ crackPassword str <$> readPermutations contents
