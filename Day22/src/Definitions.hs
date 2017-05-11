module Definitions where

import Data.Bits
import Data.Array.IArray

-- | Alias for non-negative coordinate
type Coord = Word

-- | Alias for location coordinates
type Location = (Coord, Coord)

-- | Size of disk in terabytes
type Size = Word

-- | Disk description
data Disk = Disk { location :: Location
                 , size :: Size
                 , used :: Size
                 , available :: Size }
          deriving (Show, Eq, Ord)

-- | Disk status is a word containing size, used and available stats
newtype DiskStat = DiskStat Word
                 deriving (Eq, Ord)

sizeMask :: Word
sizeMask = 1023

availShift :: Int
availShift = 10

availMask :: Word
availMask = sizeMask `shiftL` availShift

goalShift :: Int
goalShift = 2 * availShift

goalMask :: Word
goalMask = 1 `shiftL` goalShift

statSize :: DiskStat -> Size
statSize (DiskStat d) = d .&. sizeMask

statAvail :: DiskStat -> Size
statAvail (DiskStat d) = (d .&. availMask) `shiftR` availShift
 
type Grid = Array (Int, Int) DiskStat
