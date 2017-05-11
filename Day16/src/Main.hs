module Main where

import Data.List (span)

-- | Better to work with custom type than with strings
data Element = Zero | One
  deriving (Eq)

-- | For display purposes
instance Show Element where
  show Zero = "0"
  show One  = "1"
  showList lst out = concat (show <$> lst) ++ out

-- | For easier reading
instance Read Element where
  readsPrec _ (c:rest) | c == '1'  = [(One, rest)]
                       | c == '0'  = [(Zero, rest)]
                       | otherwise = []
  readsPrec _ []                   = []
  readList str = let (els, rest) = span isEl str
                 in if null els
                    then []
                    else [(toEl <$> els, rest)]
    where isEl c = c `elem` ['0', '1']
          toEl '1' = One
          toEl '0' = Zero
          toEl _   = undefined

-- | We mostly work with lists of elements.
type Elements = [Element]

-- | Generate list of elements of desired length
generate :: Int -> Elements -> Elements
generate c = go 
  where go els = let els' = els ++ [Zero] ++ (complement <$> reverse els)
                 in if length els' > c
                    then take c els'
                    else go els'
        complement Zero = One
        complement One  = Zero

-- | Computes a checksum of given elements
checksum :: Elements -> Elements
checksum [] = []
checksum els | odd (length els) = els
             | otherwise        = checksum (condense els)
  where condense [] = []
        condense [a] = [a]
        condense (a:b:rest) | a == b    = One : condense rest
                            | otherwise = Zero : condense rest
  

-- | Fills disk of given size with given seed and returns the fill and checksum
fillDisk :: Int -> Elements -> (Elements, Elements)
fillDisk s els = let fl = generate s els
                 in (fl, checksum fl)

main :: IO ()
main = putStrLn "Hello, Haskell!"
