module Main where

import Data.Ord (comparing)
import Data.List (maximumBy, minimumBy)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import System.Environment (getArgs)

-- | Alias for readability
type Count = Int

main :: IO ()
main = do
  [path] <- getArgs 
  ls <- readLines path
  print (decode mostCommonLetter ls,
          decode leastCommonLetter ls)

-- | Reads all the lines into list of lines (but vertical
--   not horizontal
readLines :: FilePath -> IO [Text]
readLines filePath = do
  ls <- T.lines <$> IO.readFile filePath
  return $ T.transpose ls

-- | Collects statistics about letters in text.
collectStats :: Text -> Map Char Count 
collectStats = T.foldl' go M.empty 
  where go :: Map Char Count -> Char -> Map Char Count
        go m c = M.alter addOne c m
        addOne :: Maybe Count -> Maybe Count
        addOne (Just n) = Just $ n + 1
        addOne Nothing  = Just 1

-- | Computes the most common letter for given text,
--   returns Nothing for empty strings.
mostCommonLetter :: Text -> Maybe Char
mostCommonLetter = pickLetter (Just . maximumBy (comparing snd))

-- | Computes the least common letter for given text,
--   returns Nothing for empty strings.
leastCommonLetter :: Text -> Maybe Char
leastCommonLetter = pickLetter (Just . minimumBy (comparing snd))

-- | Picks letter from string based on count (and given function)
pickLetter :: ([(Char, Count)] -> Maybe (Char, Count)) -> Text -> Maybe Char
pickLetter f text = let counts = collectStats text
                        in if M.null counts
                           then Nothing
                           else fst <$> f (M.toList counts)

-- | Decodes message from given pieces using given decode method.
decode :: (Text -> Maybe Char) -> [Text] -> Maybe String
decode method ls = sequence $ method <$> ls
