module Main where

import System.Environment(getArgs)
import Data.List (sortBy, foldl', isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO
import qualified Data.Text.Read  as TR

main :: IO ()
main = do
    [path] <- getArgs
    rooms <- readInput path
    let rooms' = realRooms <$> rooms
    print (sumSectors <$> rooms', filter northPoleObjects <$> rooms')

type Error = String
type Encrypted = String
type Sector = Int
type Checksum = String

data Room = Room Encrypted Sector Checksum
    deriving (Show)

splitter = T.pack "-"
bracket = T.pack "["

readRoom :: T.Text -> Either Error Room
readRoom text = let (encrypted, rest) = T.breakOnEnd splitter text
                    encrypted' = T.dropEnd 1 encrypted
                    (sector, rest') = T.breakOnEnd bracket rest
                    checksum = T.dropEnd 1 rest'
                in do
                    (sector', _) <- TR.decimal sector
                    return $ Room (T.unpack encrypted') sector' (T.unpack checksum)

readRooms :: T.Text -> Either Error [Room]
readRooms text = sequence $ readRoom <$> T.lines text

readInput :: FilePath -> IO (Either Error [Room])
readInput path = do
    contents <- TIO.readFile path
    return $ readRooms contents

checkRoom :: Room -> Bool
checkRoom (Room encrypted _ checksum) = computeChecksum encrypted == checksum

computeChecksum :: Encrypted -> Checksum
computeChecksum str = let m = collect str 
                          elems = filter ((/= '-') . fst) $ Map.toDescList m 
                      in take 5 $ joinChecksum elems
  where joinChecksum elems = fst <$> sortBy cmp elems
            where cmp (a, ac) (b, bc) = case compare ac bc of
                                         EQ -> compare a b
                                         LT -> GT
                                         GT -> LT

collect :: Encrypted -> Map.Map Char Int
collect str = go str Map.empty
  where go ""       m = m
        go (a:rest) m = go rest (Map.alter up a m)
        up (Just c) = Just (c + 1)
        up Nothing  = Just 1

realRooms :: [Room] -> [Room]
realRooms = filter checkRoom

sumSectors :: [Room] -> Int
sumSectors = foldl' go 0
  where go c (Room _ s _) = c + s

rotateLetter :: Int -> Char -> Char
rotateLetter _ '-' = ' '
rotateLetter 0  l  = l
rotateLetter n 'z' = rotateLetter (n - 1) 'a'
rotateLetter n  l  = rotateLetter (n - 1) (succ l)

decryptRoom :: Room -> String
decryptRoom (Room encrypted sector _) = rotateLetter sector <$> encrypted

northPoleObjects :: Room -> Bool
northPoleObjects r = isInfixOf "northpole object storage" $ decryptRoom r
