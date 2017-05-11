{-# LANGUAGE BangPatterns #-}
module Main where

import Debug.Trace
import Data.Array.IArray
import System.Environment (getArgs)

type Elf = Int

type Elfs = Array Int Int

remaining :: Elfs -> Elf
remaining !elfs | rangeSize (bounds elfs) == 1 = elfs ! 0 
                | otherwise                    = remaining elfs'
  where (l, u) = bounds elfs
        mid    = (u - l - 1) `div` 2
        elfs'  = ixmap (l, u - 1) mapper elfs
        mapper i | i < mid               = i + 1
                 | i >= mid && i < u - 1 = i + 2
                 | i == u - 1            = 0
                 | otherwise             = trace ("Wut? " ++ show i ++ " " ++ show l ++ " " ++ show u) $ undefined

remainingList :: [Elf] -> Elf
remainingList lst = remaining $ listArray (0, length lst - 1) lst

remTill :: Elf -> Elf
remTill n = remainingList [1..n]

main :: IO ()
main = do
  [num] <- getArgs
  print $ remainingList [1.. (read num)]
