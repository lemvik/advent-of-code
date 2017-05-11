module Parser where

import Data.Text (Text)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Array.Unboxed
import qualified Data.Text as T
import Definitions

-- | Reads cell type from char
readCellType :: Char -> Maybe CellType
readCellType c | c == '#' = Just wall
               | c == '.' = Just path
               | c `elem` ['0'..'9'] = Just $ read [c]
               | otherwise = Nothing

-- | Reads labyrinth from given string
readLabyrinth :: Text -> Maybe Labyrinth
readLabyrinth t = let lns = zip [0..] (T.lines t)
                      els = sequence $ concatMap enum lns
                  in do
                     els' <- els
                     return $ array ((0, 0), bnds els') els'
  where enum :: (Int, Text) -> [Maybe ((Int, Int), CellType)]
        enum (i, ln) = let ln' = T.unpack ln
                       in zipWith (toCell i) [0..] ln'
        toCell :: Int -> Int -> Char -> Maybe ((Int, Int), CellType)
        toCell i j c = (\c' -> ((i, j), c')) <$> readCellType c 
        bnds :: [((Int, Int), CellType)] -> (Int, Int)
        bnds els' = (fst' $ maximumBy (comparing snd') els', snd' $ maximumBy (comparing fst') els')
        fst' ((x, _), _) = x
        snd' ((_, x), _) = x
