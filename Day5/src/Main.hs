module Main where

import           Control.Monad.State.Lazy
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import           Data.Digest.Pure.MD5
import           Data.List (isPrefixOf, find)
import           Data.Maybe (fromJust)
import           System.Environment (getArgs)

-- | Type representing a entry 
data ValidHash = VH { getPosition :: Int
                    , getKey      :: ByteString
                    , getHash     :: String
                    }
  deriving (Show, Eq)

-- | Type representing candidate for hash.
data Candidate = Cand { getSource   :: ByteString
                      , getCandHash :: String
                      }
  deriving (Show, Eq)

-- | Type representing search prediate to use.
type SearchPredicate = Candidate -> Bool

-- | Type representing list of taken positions
type Positions = [Int]

-- | Type representing function that creates predicate based on
-- | claimed positions.
type PredicateMaker = Positions -> SearchPredicate

-- | Functions that updates positions based on new found hash
type PositionsUpdater = Positions -> ValidHash -> Positions

-- | Type representing state of the search - a means results to far, Set 
-- | contains found positions.
type SearchState a = State Positions a

-- | Prefix and starting position to simplify signatures
type SearchFrom = (ByteString, Int)

-- | Search conditions representing starting prefix, starting point and count
type SearchConditions = (ByteString, Int, Int)

-- | Prefix required to be present in order for hash to be valid.
requiredPrefix :: String
requiredPrefix = "00000"

-- | Computes candidate from prefix and number
makeCandidate :: ByteString -> Int -> Candidate
makeCandidate prefix num = let source = B.append prefix $ BC.pack $ show num
                           in Cand source $ show $ md5 source

-- | Finds next valid hash for prefix starting from given position using
-- | predicate from state, returning possibly modified predicate into state.
findNextValidHash :: PredicateMaker -> PositionsUpdater -> SearchFrom -> SearchState ValidHash
findNextValidHash maker updater (prefix, position) = do
  positions <- get
  let predicate  = maker positions . snd
      result     = make $ fromJust $ find predicate $ makeCandidate' prefix <$> [position..] -- fromJust is justified since it's known that such hash exists.
      positions' = updater positions result
  put positions'
  return result
  where makeCandidate' pref pos = (pos, makeCandidate pref pos)
        make (pos, Cand source h) = VH pos source h

-- | Finds given count valid hashes for prefix starting from position and matching predicate
findValidHashes :: PredicateMaker -> PositionsUpdater -> SearchConditions -> SearchState [ValidHash]
findValidHashes _     _       (_     , _    , 0)     = return []
findValidHashes maker updater (prefix, start, count) = do
  vh@(VH pos _ _) <- findNextValidHash maker updater (prefix, start)
  other <- findValidHashes maker updater (prefix, pos, count - 1)
  return $ vh:other

-- | Find hashes without regard for already found positions and with constant predicate
findSimple :: SearchConditions -> SearchState [ValidHash]
findSimple = findValidHashes maker updater
  where maker :: PredicateMaker
        maker = const checkPrefix
        updater :: PositionsUpdater
        updater pos _ = pos ++ [length pos]


-- | Default predicate used
checkPrefix :: SearchPredicate
checkPrefix (Cand _ str) = requiredPrefix `isPrefixOf` str

-- | Extracts position from given hash string
extractPosition :: String -> Maybe Int
extractPosition str = if length str < 6
                      then Nothing
                      else let n = take 1 . drop 5 $ str
                           in case n of
                                [a] | a `elem` ['0'..'7'] -> Just $ read n
                                _                         -> Nothing


-- | Predicate that checks if candidate has proper position
checkPositions :: Positions -> SearchPredicate
checkPositions positions = pr
  where pr :: SearchPredicate
        pr (Cand _ str) = case extractPosition str of
          Just pos -> pos `notElem` positions
          Nothing  -> False

-- | Finds hashes using positions of previously found letters to skip unnecessary hashes
findUsingPositions :: SearchConditions -> SearchState [ValidHash]
findUsingPositions = findValidHashes maker updater
  where maker :: PredicateMaker
        maker positions v = checkPrefix v && checkPositions positions v
        updater :: PositionsUpdater
        updater positions (VH _ _ str) = case extractPosition str of
          Just pos -> pos:positions
          Nothing  -> positions

main :: IO ()
main = do
  [method, prefix, _start, _count] <- getArgs
  let start = read _start :: Int
  let count = read _count :: Int
  let f = case method of
            "simple"    -> findSimple
            "positions" -> findUsingPositions
            _           -> \_ -> return []
  print $ runState (f (BC.pack prefix, start, count)) []
