module Main where

import           Debug.Trace
import           Data.List (sort)
import qualified Data.Text.IO as IO
import qualified Data.Text.Read as R
import           Data.Text (Text)
import qualified Data.Text as T
import           System.Environment (getArgs)

-- | Span of available addresses
data Span = Span { minAddress :: Int
                 , maxAddress :: Int }
          deriving (Show, Eq, Ord)

-- | Tries to create a span that unites two given spans - only possible if
--   they are intersecting
union :: Span -> Span -> Maybe Span
union (Span sa ea) (Span sb eb) | sb > ea + 1 || sa > eb + 1 = Nothing
                                | otherwise                  = Just (Span (min sa sb) (max ea eb))

-- | Returns the minimal address not covered by given spans
minimalAddress :: [Span] -> Int
minimalAddress []          = 0
minimalAddress [Span _ mx] = mx + 1
minimalAddress (a:b:rest)  = case a `union` b of
                               Just c  -> minimalAddress (c:rest)
                               Nothing -> trace ("Candidate: " ++ show a ++ ", next: " ++ show b) $ 1 + maxAddress a


-- | Counts up all IPs allowed by blacklist
allowedIps :: [Span] -> Int -> Int 
allowedIps spans maxIP = go spans 0
  where go [] acc          = acc
        go [Span _ mx] acc = acc + (maxIP - mx)
        go (a:b:rest) acc  = case a `union` b of
                               Just c  -> go (c:rest) acc
                               Nothing -> go (b:rest) (acc + (minAddress b - maxAddress a - 1))

-- | Separator used in input
separator :: Text
separator = T.pack "-"

-- | Reads spans from input stream
readSpans :: Text -> Either String [Span]
readSpans t = sort <$> sequence (readSpan <$> T.lines t)
  where readSpan l = let (start, end) = T.breakOn separator l
                     in do
                        (start', _) <- R.decimal start
                        (end', _)   <- R.decimal (T.drop 1 end)
                        return $ Span start' end'

main :: IO ()
main = do
  [path] <- getArgs
  content <- IO.readFile path
  print $ flip allowedIps 4294967295 <$> readSpans content
