module Main where

import Data.Foldable (find)

-- | Type describing the disc
data Disc = Disc { number :: Int
                 , positions :: Int
                 , initial :: Int } 
          deriving (Show, Eq)

tests :: Int -> Bool
tests n = let n13 = 13 * n
          in n13 - 9 `mod` 17 == 0 && 
             n13 - 9 `mod` 7 == 0 &&
             n13 - 6 `mod` 19 == 0 &&
             n13 - 7 `mod` 5 == 0 &&
             n13 - 6 `mod` 3 == 0

findMatching :: Int -> Maybe Int
findMatching start = find tests [start..]


main :: IO ()
main = putStrLn "Hello, Haskell!"
