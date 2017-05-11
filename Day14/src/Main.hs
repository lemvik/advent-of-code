module Main where

import           Data.List (isInfixOf, partition)
import qualified Data.ByteString.Lazy.Char8 as BC
import           Data.Digest.Pure.MD5
import           System.Environment (getArgs)



-- | Candidate for valid hash 
data Candidate = Candidate { candidatePosition :: Int
                           , candidateLetter :: Char
                           , candidateHash :: String }
               deriving (Show, Eq)

-- | Valid hash type
data ValidHash = VHash { hashPosition :: Int
                       , matchPosition :: Int
                       , hashLetter :: Char
                       , validHash :: String }
               deriving (Show, Eq)

data SearchState = SS { currentIndex :: Int
                      , salt :: String
                      , candidates :: [Candidate]
                      , hashes :: [ValidHash]
                      , hashesCount :: Int }
                 deriving (Show, Eq)

-- | Computes hash for given salt and number
computeHash :: String -> Int -> String 
computeHash str num = show $ md5 (BC.pack $ str ++ show num)

-- | Computes *secure* hash by re-hashing given times
computeSecureHash :: Int -> String -> Int -> String
computeSecureHash n str num = go n $ computeHash str num
  where go 0 s = s
        go i s = go (i - 1) (show . md5 $ BC.pack s)

-- | Checks if string has a triplet and returns just char which form the triplet
--   if it is 
hasTriplet :: String -> Maybe Char
hasTriplet (a:rest@(b:c:_)) | a == b && b == c = Just a
                            | otherwise        = hasTriplet rest
hasTriplet _                                   = Nothing     

-- | Returns true if string contains five given letters in a row
hasFive :: Char -> String -> Bool
hasFive c str = replicate 5 c `isInfixOf` str

-- | Initial state of the search
initialState :: String -> SearchState
initialState s = SS 0 s [] [] 0

-- | Type for hasher 
type Hasher = String -> Int -> String

-- | Perform a single step of search for given 
step :: Hasher -> SearchState -> SearchState
step hasher st@(SS i s cs hs hsc) = let str = hasher s i
                                    in case hasTriplet str of -- If str has five letters in a row it will have triplet too
                                         Just c  -> process str c
                                         Nothing -> st { currentIndex = i + 1 }
  where process :: String -> Char -> SearchState
        process str c = let cs'         = filter ((>= i - 1000) . candidatePosition) cs
                            (hss, cs'') = partition (matches str) cs'
                            cs'''       = Candidate i c str : cs''
                            (hs', hsc') = if not (null hss)
                                          then let hsh = (candidateToHash i <$> hss) ++ hs
                                               in (hsh, length hsh)
                                          else (hs, hsc)
                        in SS (i + 1) s cs''' hs' hsc'
        matches str (Candidate _ c _) = hasFive c str
        candidateToHash p (Candidate i' c st') = VHash i' p c st'

-- | Steps starting with initial search state until predicate starts to hold
search :: Hasher -> (SearchState -> Bool) -> SearchState -> SearchState
search hasher pr state = if pr state
                         then state
                         else search hasher pr (step hasher state)
                            
-- | Returns true if number of found hashes is equal to 
hashesFound :: Int -> SearchState -> Bool
hashesFound num (SS _ _ _ _ hsc) = num <= hsc

-- | Limits the number of steps taken during search
searched :: Int -> SearchState -> Bool
searched num (SS i _ _ _ _) = num <= i

main :: IO ()
main = do
  [slt, num] <- getArgs
  print $ hashPosition <$> hashes (search (computeSecureHash 2016) (hashesFound $ read num) (initialState slt))
