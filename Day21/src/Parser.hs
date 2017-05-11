module Parser where

import Definitions
import Text.Parsec
import Text.Parsec.Text
import Data.Text (Text)
import qualified Data.Text as T

-- | Parses type of rotation
parseRotation :: Parser Rotation
parseRotation = (string "left" >> return RotateLeft) <|> (string "right" >> return RotateRight)

-- | Parses swap position command
parseSwapPosition :: Parser Permutation
parseSwapPosition = do
  _ <-string "swap position "
  posA <- read <$> many1 digit
  _ <-string " with position "
  posB <- read <$> many1 digit
  return $ SwapPosition posA posB

-- | Parses swap letters command
parseSwapLetters :: Parser Permutation
parseSwapLetters = do
  _ <-string "swap letter "
  letterA <- letter
  _ <-string " with letter "
  letterB <- letter
  return $ SwapLetters letterA letterB

-- | Parses rotation command
parseRotate :: Parser Permutation
parseRotate = do
  _ <-string "rotate "
  dir <- parseRotation
  spaces
  cnt <- read <$> many1 digit
  return $ Rotate dir cnt

-- | Parses relative rotation command
parseRelativeRotation :: Parser Permutation
parseRelativeRotation = do
  _ <-string "rotate based on position of letter "
  l <- letter
  return $ RotateRelative l

-- | Parses reversal
parseReverse :: Parser Permutation
parseReverse = do
  _ <-string "reverse positions "
  posA <- read <$> many1 digit
  _ <-string " through "
  posB <- read <$> many1 digit
  return $ Reverse posA posB

-- | Parses move command
parseMove :: Parser Permutation
parseMove = do
  _ <-string "move position "
  posA <- read <$> many1 digit
  _ <-string " to position "
  posB <- read <$> many1 digit
  return $ Move posA posB

-- | Parses any permutation
parsePermutation :: Parser Permutation
parsePermutation = choice [ try parseSwapPosition
                          , try parseSwapLetters
                          , try parseRotate
                          , try parseRelativeRotation
                          , try parseReverse
                          , try parseMove ]

-- | Reads permutations from given text
readPermutations :: Text -> Either ParseError [Permutation]
readPermutations src = sequence $ parse parsePermutation "text" <$> T.lines src
