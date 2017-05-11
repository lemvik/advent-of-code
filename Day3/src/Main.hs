module Main where

import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

main :: IO ()
main = do
    [filePath] <- getArgs
    contents <- TIO.readFile filePath
    print $ do
        sides <- readInput contents
        sides' <- transformSides sides
        return (length $ filter isTriangle sides, length $ filter isTriangle sides')

type Sides = (Int, Int, Int)

readSides :: T.Text -> Either String Sides
readSides text = let wrds = T.words text
                 in case wrds of
                    [a, b, c] -> do
                        (a', _) <- TR.decimal a
                        (b', _) <- TR.decimal b
                        (c', _) <- TR.decimal c
                        return (a', b', c')
                    _         -> Left "Unable to read sides"

readInput :: T.Text -> Either String [Sides]
readInput text = sequence $ readSides <$> T.lines text

isTriangle :: Sides -> Bool
isTriangle (a, b, c) = a < b + c && b < a + c && c < a + b

sidesToArrays :: [Sides] -> ([Int], [Int], [Int])
sidesToArrays sides = go sides ([], [], [])
    where go [] (as, bs, cs)               = (reverse as, reverse bs, reverse cs)
          go ((a, b, c):rest) (as, bs, cs) = go rest (a:as, b:bs, c:cs)

listToSides :: [Int] -> Either String [Sides]
listToSides sides = go sides []
    where go [] acc = Right . reverse $ acc
          go [_] _ = Left "Size of list is not a multiple of 3"
          go [_,_] _ = Left "Size of list is not a multiple of 3"
          go (a:b:c:rest) acc = go rest ((a, b, c):acc)

liftT :: (a -> b) -> (a, a, a) -> (b, b, b)
liftT f (a, b, c) = (f a, f b, f c)

extractT :: (Monad m) => (m a, m a, m a) -> m (a, a, a)
extractT (a, b, c) = do
    a' <- a
    b' <- b
    c' <- c
    return (a', b', c')

transformSides :: [Sides] -> Either String [Sides]
transformSides sides = do
    (a, b, c) <- extractT $ liftT listToSides $ sidesToArrays sides
    return $ a ++ b ++ c