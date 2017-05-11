module Definitions where

-- | Position is just a numebr
type Position = Int

-- | Direction of rotation
data Rotation = RotateLeft | RotateRight
              deriving (Show, Eq, Ord)

-- | Type of operation to perform
data Permutation = SwapPosition { fromPosition :: Position, toPosition :: Position }
                 | SwapLetters { sourceLetter :: Char, targetLetter :: Char }
                 | Rotate { direction :: Rotation, count :: Int } 
                 | RotateRelative { fromLetter :: Char }
                 | Reverse { fromPosition :: Position, toPosition :: Position }
                 | Move { fromPosition :: Position, toPosition :: Position }
                 deriving (Show, Eq, Ord)
