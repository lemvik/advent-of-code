module Main where

import Data.Char (isSpace)
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import Data.List (break, splitAt)
import System.Environment (getArgs)

main :: IO ()
main = do
  [path] <- getArgs
  str <- T.unpack <$> IO.readFile path
  print $ estimateSize $ filter (not . isSpace) str

-- | Uncompresses string given by input.
uncompressString :: String -> String
uncompressString str = concat $ uncompressChunks str

-- | Marker definition
data Marker = Marker { getLength :: Int
                     , getCount  :: Int
                     }
  deriving (Show, Eq)


-- | Reads marker from the string.
readMarker :: String -> (Marker, String)
readMarker s = let (len, rest) = break (== 'x') (drop 1 s)
                   (cnt, rest') = break (== ')') (drop 1 rest)
                   rest'' = drop 1 rest'
               in (Marker (read len) (read cnt), rest'')

-- | Uncompress string into chunks
uncompressChunks :: String -> [String]
uncompressChunks str = go str [] []
  where go []         acc chunk = reverse (reverse chunk:acc)
        go s@(c:rest) acc chunk | c == '('  = let (marker, rest') = readMarker s
                                                  (chunks, rest'') = processMarker marker rest'
                                              in go rest'' (chunks ++ reverse chunk:acc) []
                                | otherwise = go rest acc (c:chunk)
        processMarker (Marker len cnt) s = let (chunk, rest) = splitAt len s
                                           in (replicate cnt chunk, rest)

-- | Estimates size of uncompressed string. 
estimateSize :: String -> Int
estimateSize str = go str 0
  where go []         acc             = acc
        go s@(c:rest) acc | c == '('  = let (marker, rest') = readMarker s
                                            (size, rest'') = processMarker marker rest'
                                        in go rest'' (size + acc)
                          | otherwise = go rest (acc + 1)
        processMarker (Marker len cnt) s = let (chunk, rest) = splitAt len s
                                           in (cnt * estimateSize chunk, rest)
